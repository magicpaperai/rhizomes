const {Interval, SourceRhizome, LinkRhizome, Rhizomes} = require('../dist')

class SimpleBasis {
  constructor(id) {
    this.id = id
  }
  readRange(start, end) {
    return ''
  }
}

test('Interval#toString', () => {
  const basis = new SimpleBasis('a')
  const interval = new Interval(basis, 0, 1)
  expect(interval.toString()).toBe('a:0-1')

  const basis2 = new SimpleBasis('b')
  const interval2 = new Interval(basis2, 1, 10)
  expect(interval2.toString()).toBe('b:1-10')
})

describe('Rhizomes#compose', () => {
  const a = new SimpleBasis('a')
  const b = new SimpleBasis('b')
  const c = new SimpleBasis('c')

  test('simple compose', () => {
    const ab = new SourceRhizome(
      new Interval(a, 0, 5),
      new Interval(b, 0, 5)
    )
    const bc = new SourceRhizome(
      new Interval(b, 0, 10),
      new Interval(c, 0, 10)
    )
    const abs = new Rhizomes([ab])
    const bcs = new Rhizomes([bc])
    expect((abs.compose(bcs).toString())).toBe('source: a:0-5 -> c:0-5')
  })

  test('simple compose with a LinkRhizome', () => {
    const ab = new SourceRhizome(
      new Interval(a, 0, 5),
      new Interval(b, 5, 10)
    )
    const srcbc = new SourceRhizome(
      new Interval(b, 0, 10),
      new Interval(c, 10, 20)
    )
    const linkbc = new LinkRhizome(
      new Interval(b, 5, 6),
      new Interval(c, 0, 10)
    )
    const abs = new Rhizomes([ab])
    const bcs = new Rhizomes([srcbc], [linkbc])
    const composed = abs.compose(bcs).toString()
    expect(composed).toBe([
      'source: a:0-5 -> c:15-20',
      'link: a:0-1 -> c:0-10',
    ].join('\n'))
  })

  test('link rhizomes are preserved under composition', () => {
    const srcab = new SourceRhizome(
      new Interval(a, 0, 5),
      new Interval(b, 5, 10)
    )
    const linkab = new LinkRhizome(
      new Interval(a, 0, 5),
      new Interval(b, 5, 10)
    )
    const srcbc = new SourceRhizome(
      new Interval(b, 0, 10),
      new Interval(c, 10, 20)
    )
    const abs = new Rhizomes([srcab], [linkab])
    const bcs = new Rhizomes([srcbc])
    const composed = abs.compose(bcs).toString()
    expect(composed).toBe([
      'source: a:0-5 -> c:15-20',
      'link: a:0-5 -> b:5-10',
    ].join('\n'))
  })
})
