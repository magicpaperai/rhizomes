import _ from 'lodash'

export interface Basis {
  id: string
  readRange(start: number, end: number): string
}

export class Interval<B extends Basis> {
  basis: B
  start: number
  end: number
  constructor(basis, start, end) {
    this.basis = basis
    this.start = start
    this.end = end
  }

  read(): string {
    return this.basis.readRange(this.start, this.end)
  }

  toString(): string {
    return this.basis.id + ':' + [this.start, this.end].join('-')
  }

  intersect(otherInterval: Interval<B>): Interval<B> {
    if (this.basis !== otherInterval.basis) return null
    const implied = new Interval<B>(
      this.basis,
      Math.max(this.start, otherInterval.start),
      Math.min(this.end, otherInterval.end)
    )
    const isEmpty = implied.end <= implied.start
    return isEmpty ? null : implied
  }

  touching(otherInterval: Interval<B>): boolean {
    if (this.basis !== otherInterval.basis) return false
    if (this.intersect(otherInterval)) return true
    if (this.start === otherInterval.end || otherInterval.start === this.end) return true
    return false
  }

  sameBasis(otherInterval: Interval<B>) {
    return this.basis === otherInterval.basis
  }

  diff2(otherInterval: Interval<B>): Interval<B>[] {
    if (this.basis !== otherInterval.basis) return [this]
    if (!this.intersect(otherInterval)) return [this]
    const startsAfter = otherInterval.start > this.start
    const endsBefore = otherInterval.end < this.end
    if (endsBefore && startsAfter) {
      return [
        new Interval(this.basis, this.start, otherInterval.start),
        new Interval(this.basis, otherInterval.end, this.end),
      ]
    }
    if (startsAfter) return [new Interval(this.basis, this.start, otherInterval.start)]
    if (endsBefore) return [new Interval(this.basis, otherInterval.end, this.end)]
    return []
  }
}

function diffIntervals<B extends Basis>(xs: Interval<B>[], ys: Interval<B>[]): Interval<B>[] {
  if (_.isEmpty(xs)) return []
  if (_.isEmpty(ys)) return xs
  if (xs.length === 1) return diffIntervals(xs[0].diff2(ys[0]), _.tail(ys))
  return [...diffIntervals([xs[0]], ys), ...diffIntervals(_.tail(xs), ys)]
}

function sortIntervals<B extends Basis>(addrs: Interval<B>[]): Interval<B>[] {
  const bases = _.map(addrs, 'basis')
  return _.orderBy(addrs, [x => bases.indexOf(x.basis), 'start'], ['asc', 'asc'])
}

function normalizeIntervals<B extends Basis>(addrs: Interval<B>[]): Interval<B>[] {
  let grouped = new Map<B, Interval<B>[]>()
  const sorted = sortIntervals(addrs)
  sorted.forEach(addr => {
    grouped.set(addr.basis, [...(grouped.get(addr.basis) || []), addr])
  })
  return _.flatMap(Array.from(grouped.entries()), ([b, xs]: [any, Interval<B>[]]) => {
    return xs.reduce((a, x) => {
      if (_.isEmpty(a)) return [x]
      if (_.last(a).touching(x)) {
        const y = _.last(a)
        return [..._.initial(a), new Interval(y.basis, Math.min(x.start, y.start), Math.max(x.end, y.end))]
      }
      return [...a, x]
    }, [])
  })
}

export class Ribbon<B extends Basis> {
  origin: Interval<B>
  dest: Interval<B>
  constructor(origin: Interval<B>, dest: Interval<B>) {
    this.origin = origin
    this.dest = dest
  }

  toString(): string {
    return [this.origin.toString(), this.dest.toString()].join(' -> ')
  }

  invert() {
    return new Ribbon(this.dest, this.origin)
  }

  translate(point: number): number {
    return point - this.origin.start + this.dest.start
  }

  translateI(addr: Interval<B>): Interval<B> {
    if (this.origin.basis !== addr.basis) return null
    const extrapolate = new Interval<B>(this.dest.basis, this.translate(addr.start), this.translate(addr.end))
    return extrapolate.intersect(this.dest)
  }

  partialI(addr: Interval<B>): Ribbon<B> {
    const translated = this.translateI(addr)
    const untranslated = this.invert().translateI(translated)
    return translated ? new Ribbon(untranslated, translated) : null
  }
}

export class Ribbons<B extends Basis> {
  ribbons: Ribbon<B>[]
  constructor(ribbons: Ribbon<B>[]) {
    this.ribbons = _.compact(ribbons)
  }

  isEmpty(): boolean {
    return _.isEmpty(this.ribbons)
  }

  toString(): string {
    return this.ribbons.map(x => x.toString()).join('\n')
  }

  invert(): Ribbons<B> {
    return new Ribbons<B>(this.ribbons.map(x => x.invert()))
  }

  image(addrs: Interval<B>[]): Interval<B>[] {
    return normalizeIntervals(_.compact(_.flatMap(this.ribbons, link =>
      _.flatMap(addrs, addr => link.translateI(addr))
    )))
  }

  preimage(addrs: Interval<B>[]): Interval<B>[] {
    return this.invert().image(addrs)
  }

  domain(): Interval<B>[] {
    return normalizeIntervals(_.map(this.ribbons, 'origin'))
  }

  range(): Interval<B>[] {
    return normalizeIntervals(_.map(this.ribbons, 'dest'))
  }

  partial(addr: Interval<B>): Ribbons<B> {
    const overlapping = this.ribbons.filter(link => link.origin.intersect(addr))
    return new Ribbons(overlapping.map(link => link.partialI(addr)))
  }

  prism(otherRibbons: Ribbons<B>, addr: Interval<B>): Ribbons<B> {
    const forwardRibbons = this.partial(addr).ribbons
    const backwardRibbons = otherRibbons.invert().partial(addr).ribbons
    return new Ribbons(_.flatMap(forwardRibbons, forward =>
      _.flatMap(backwardRibbons, backward => new Ribbon(backward.dest, forward.dest))
    ))
  }

  compose(otherRibbons: Ribbons<B>): Ribbons<B> {
    const longI = otherRibbons.image(this.range())
    const prelongs = otherRibbons.preimage(longI)
    const shortI = diffIntervals(this.range(), prelongs)
    const preshorts = this.preimage(shortI)
    const shorts = _.flatMap(preshorts, addr => this.partial(addr).ribbons)
    const longs = _.flatMap(prelongs, addr => otherRibbons.prism(this, addr).ribbons)
    return new Ribbons([...shorts, ...longs])
  }
}
