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

export class Rhizome<B extends Basis> {
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
    return new Rhizome(this.dest, this.origin)
  }

  translate(point: number): number {
    return point - this.origin.start + this.dest.start
  }

  translateI(addr: Interval<B>): Interval<B> {
    if (this.origin.basis !== addr.basis) return null
    const extrapolate = new Interval<B>(this.dest.basis, this.translate(addr.start), this.translate(addr.end))
    return extrapolate.intersect(this.dest)
  }

  partialI(addr: Interval<B>): Rhizome<B> {
    const translated = this.translateI(addr)
    const untranslated = this.invert().translateI(translated)
    return translated ? new Rhizome(untranslated, translated) : null
  }
}

export class Rhizomes<B extends Basis> {
  rhizomes: Rhizome<B>[]
  constructor(rhizomes: Rhizome<B>[]) {
    this.rhizomes = _.compact(rhizomes)
  }

  isEmpty(): boolean {
    return _.isEmpty(this.rhizomes)
  }

  toString(): string {
    return this.rhizomes.map(x => x.toString()).join('\n')
  }

  invert(): Rhizomes<B> {
    return new Rhizomes<B>(this.rhizomes.map(x => x.invert()))
  }

  image(addrs: Interval<B>[]): Interval<B>[] {
    return normalizeIntervals(_.compact(_.flatMap(this.rhizomes, link =>
      _.flatMap(addrs, addr => link.translateI(addr))
    )))
  }

  preimage(addrs: Interval<B>[]): Interval<B>[] {
    return this.invert().image(addrs)
  }

  domain(): Interval<B>[] {
    return normalizeIntervals(_.map(this.rhizomes, 'origin'))
  }

  range(): Interval<B>[] {
    return normalizeIntervals(_.map(this.rhizomes, 'dest'))
  }

  partial(addr: Interval<B>): Rhizomes<B> {
    const overlapping = this.rhizomes.filter(link => link.origin.intersect(addr))
    return new Rhizomes(overlapping.map(link => link.partialI(addr)))
  }

  prism(otherRhizomes: Rhizomes<B>, addr: Interval<B>): Rhizomes<B> {
    const forwardRhizomes = this.partial(addr).rhizomes
    const backwardRhizomes = otherRhizomes.invert().partial(addr).rhizomes
    return new Rhizomes(_.flatMap(forwardRhizomes, forward =>
      _.flatMap(backwardRhizomes, backward => new Rhizome(backward.dest, forward.dest))
    ))
  }

  compose(otherRhizomes: Rhizomes<B>): Rhizomes<B> {
    const longI = otherRhizomes.image(this.range())
    const prelongs = otherRhizomes.preimage(longI)
    const shortI = diffIntervals(this.range(), prelongs)
    const preshorts = this.preimage(shortI)
    const shorts = _.flatMap(preshorts, addr => this.partial(addr).rhizomes)
    const longs = _.flatMap(prelongs, addr => otherRhizomes.prism(this, addr).rhizomes)
    return new Rhizomes([...shorts, ...longs])
  }
}
