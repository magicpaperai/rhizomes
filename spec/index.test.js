const {Interval, SourceRhizome, LinkRhizome, Rhizomes} = require('../dist')

class SimpleBasis {
  constructor(id) {
    this.id = id
  }
  readRange(start, end) {
    return ['']
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

  test('compose two quote collapse block edits', () => {
    const ab1 = new SourceRhizome(
      new Interval(b, 0, 10),
      new Interval(a, 0, 10)
    )
    const ab2 = new SourceRhizome(
      new Interval(b, 11, 21),
      new Interval(a, 20, 30)
    )

    const bc1 = new SourceRhizome(
      new Interval(c, 0, 10),
      new Interval(b, 0, 10)
    )
    const bc2 = new SourceRhizome(
      new Interval(c, 10, 11),
      new Interval(b, 10, 11)
    )
    const abs = new Rhizomes([ab1, ab2], [])
    const bcs = new Rhizomes([bc1, bc2], [])
    const composed = bcs.compose(abs).toString()
    expect(composed).toBe([
      'source: c:10-11 -> b:10-11',
      'source: c:0-10 -> a:0-10',
    ].join('\n'))
  })

  test('compose two quote collapse block edits w extra paragraph', () => {
    const ab1 = new SourceRhizome(
      new Interval(b, 0, 10),
      new Interval(a, 0, 10)
    )
    const ab2 = new SourceRhizome(
      new Interval(b, 10, 20),
      new Interval(a, 10, 20)
    )
    const ab3 = new SourceRhizome(
      new Interval(b, 21, 31),
      new Interval(a, 30, 40)
    )

    const bc1 = new SourceRhizome(
      new Interval(c, 0, 10),
      new Interval(b, 0, 10)
    )
    const bc2 = new SourceRhizome(
      new Interval(c, 10, 20),
      new Interval(b, 10, 20)
    )
    const bc3 = new SourceRhizome(
      new Interval(c, 20, 21),
      new Interval(b, 20, 21)
    )
    const abs = new Rhizomes([ab1, ab2, ab3], [])
    const bcs = new Rhizomes([bc1, bc2, bc3], [])
    const composed = bcs.compose(abs, true).toString()
    expect(composed).toBe([
      'source: c:20-21 -> b:20-21',
      'source: c:0-10 -> a:0-10',
      'source: c:10-20 -> a:10-20',
    ].join('\n'))
  })
})
