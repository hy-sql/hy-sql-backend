const {
    transformSplitConditionsIntoConditionsArray,
} = require('../../commandParsers/parserTools/arrayTransformationTools')

test('transforms successfully', () => {
    const input = [
        'LENGTH',
        '(',
        'nimi',
        ')',
        '=5',
        'OR',
        '(',
        'LENGTH',
        '(',
        'nimi',
        ')',
        '<7',
        'AND',
        'hinta=4',
        ')',
    ]

    const expected = [
        'LENGTH(nimi)=5',
        'OR',
        '(',
        'LENGTH(nimi)<7',
        'AND',
        'hinta=4',
        ')',
    ]

    const output = transformSplitConditionsIntoConditionsArray(input)

    expect(output).toEqual(expected)
})
