const {
    transformOrderByInputArrayIntoOrderByFieldsArray,
} = require('../../commandParsers/parserTools/arrayTransformationTools')

test('transforms successfully', () => {
    const input = ['LENGTH', '(', 'nimi', ')', 'DESC,', 'hinta']

    const expected = [['LENGTH(nimi)', 'DESC'], ['hinta']]

    const output = transformOrderByInputArrayIntoOrderByFieldsArray(input)

    expect(output).toEqual(expected)
})
