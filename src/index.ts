import _ from 'lodash'

export interface Basis {
  id: string
  readRange(start: number, end: number): string[]
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

  read(): string[] {
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

export function normalizeIntervals<B extends Basis>(addrs: Interval<B>[]): Interval<B>[] {
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

type RhizomeKind = 'source' | 'link'
class Rhizome<B extends Basis> {
  origin: Interval<B>
  dest: Interval<B>
  kind: RhizomeKind
  constructor(origin: Interval<B>, dest: Interval<B>) {
    this.origin = origin
    this.dest = dest
  }

  toString(): string {
    return this.kind + ': ' + [this.origin.toString(), this.dest.toString()].join(' -> ')
  }
}

export class SourceRhizome<B extends Basis> extends Rhizome<B> {
  kind: RhizomeKind = 'source'
  translate(point: number): number {
    return point - this.origin.start + this.dest.start
  }

  translateI(addr: Interval<B>): Interval<B> {
    if (this.origin.basis !== addr.basis) return null
    const extrapolate = new Interval<B>(this.dest.basis, this.translate(addr.start), this.translate(addr.end))
    return extrapolate.intersect(this.dest)
  }

  partialI(addr: Interval<B>): SourceRhizome<B> {
    const translated = this.translateI(addr)
    const untranslated = this.invert().translateI(translated)
    return translated ? new SourceRhizome(untranslated, translated) : null
  }

  invert() {
    return new SourceRhizome(this.dest, this.origin)
  }
}

export class LinkRhizome<B extends Basis> extends Rhizome<B> {
  kind: RhizomeKind = 'link'

  partialI(addr: Interval<B>): LinkRhizome<B> {
    if (!addr.intersect(this.origin)) return null
    return new LinkRhizome(addr.intersect(this.origin), this.dest)
  }

  invert() {
    return new LinkRhizome(this.dest, this.origin)
  }
}

export class Rhizomes<B extends Basis> {
  sources: SourceRhizome<B>[]
  links: LinkRhizome<B>[]
  constructor(sources: SourceRhizome<B>[], links: LinkRhizome<B>[] = []) {
    if (sources.some(x => !(x instanceof SourceRhizome))) {
      throw new Error('sources must be type SourceRhizome')
    }
    if (links.some(x => !(x instanceof LinkRhizome))) {
      throw new Error('links must be type LinkRhizome')
    }
    this.sources = _.compact(sources)
    this.links = _.compact(links)
  }

  isEmpty(): boolean {
    return _.isEmpty(this.sources)
  }

  toString(): string {
    return [...this.sources, ...this.links].map(x => x.toString()).join('\n')
  }

  invert(): Rhizomes<B> {
    return new Rhizomes<B>(this.sources.map(x => x.invert()))
  }

  image(addrs: Interval<B>[]): Interval<B>[] {
    return normalizeIntervals(_.compact(_.flatMap(this.sources, src =>
      _.flatMap(addrs, addr => src.translateI(addr))
    )))
  }

  preimage(addrs: Interval<B>[]): Interval<B>[] {
    return this.invert().image(addrs)
  }

  domain(): Interval<B>[] {
    return normalizeIntervals(_.map(this.sources, 'origin'))
  }

  range(): Interval<B>[] {
    return normalizeIntervals(_.map(this.sources, 'dest'))
  }

  partial(addr: Interval<B>): Rhizomes<B> {
    const overlapping = this.sources.filter(src => src.origin.intersect(addr))
    return new Rhizomes(overlapping.map(src => src.partialI(addr)), this.links)
  }

  partialLinks(addr: Interval<B>): Rhizomes<B> {
    const overlapping = this.links.filter(link => link.origin.intersect(addr))
    return new Rhizomes(this.sources, overlapping.map(link => link.partialI(addr)))
  }

  prism(otherRhizomes: Rhizomes<B>, addr: Interval<B>): Rhizomes<B> {
    const forwardRhizomes = this.partial(addr).sources
    const backwardRhizomes = otherRhizomes.invert().partial(addr).sources
    const cartesian = _.flatMap(forwardRhizomes, forward =>
      _.flatMap(backwardRhizomes, backward => new SourceRhizome(backward.dest, forward.dest))
    )
    return new Rhizomes(_.compact(cartesian.map(rhizome => {
      const destPreimage = this.preimage([rhizome.dest])
      const originImage = otherRhizomes.image([rhizome.origin])
      const pairs = _.flatMap(destPreimage, d => originImage.map(o => [o, d]))
      const overlap = normalizeIntervals(_.compact(pairs.map(([x, y]) => x.intersect(y))))
      const overlapDest = this.image(overlap)
      const overlapOrigin = otherRhizomes.preimage(overlap)
      if (_.isEmpty(overlapDest) || _.isEmpty(overlapOrigin)) return null
      // TODO: assuming singleton lists for these may be overly simplistic
      return new SourceRhizome(overlapOrigin[0], overlapDest[0])
    })))
  }

  compose(otherRhizomes: Rhizomes<B>): Rhizomes<B> {
    const range = this.range()
    const longI = otherRhizomes.image(range)
    const prelongs = otherRhizomes.preimage(longI)
    const shortI = diffIntervals(this.range(), prelongs)
    const preshorts = this.preimage(shortI)
    const shorts = _.flatMap(preshorts, addr => this.partial(addr).sources)
    const longs = _.flatMap(prelongs, addr => otherRhizomes.prism(this, addr).sources)

    const linksOut = _.flatMap(range, addr => otherRhizomes.partialLinks(addr).links)
    const indirectLinks = _.flatMap(linksOut, link => this.preimage([link.origin]).map(pre => new LinkRhizome(pre, link.dest)))
    return new Rhizomes(
      [...shorts, ...longs],
      [
        ...this.links, // direct links preserved
        ...indirectLinks, // indirect links added
      ]
    )
  }
}
