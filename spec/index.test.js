const {Interval} = require('../dist')

test('Interval#toString', () => {
  const basis = {id: 'a', readRange(start, end) {return ''}}
  const interval = new Interval(basis, 0, 1)
  expect(interval.toString()).toBe('a:0-1')

  const basis2 = {id: 'b', readRange(start, end) {return ''}}
  const interval2 = new Interval(basis2, 1, 10)
  expect(interval2.toString()).toBe('b:1-10')
})
