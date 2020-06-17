const {
    evaluateExpression,
} = require('../../services/components/expressionTools')

test('transforms successfully', () => {
    const expressionArray = [
        { type: 'integer', value: 5 },
        { type: 'operator', value: '+' },
        { type: 'column', value: 'price' },
        { type: 'operator', value: '*' },
        { type: 'integer', value: 4 },
    ]

    const row = { id: 6, name: 'celery', price: 6, amount: 70 }

    const expected = 29

    const output = evaluateExpression(expressionArray, row)

    expect(output).toEqual(expected)
})
