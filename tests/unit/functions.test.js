const {
    executeAggregateFunction,
    executeStringFunction,
} = require('../../services/components/functions')

const rows = [
    { id: 1, nimi: 'retiisi', hinta: 7, lkm: 20 },
    { id: 2, nimi: 'porkkana', hinta: 5, lkm: 40 },
    { id: 3, nimi: 'nauris', hinta: 4, lkm: 40 },
    { id: 4, nimi: 'lanttu', hinta: 8, lkm: 20 },
    { id: 5, nimi: 'selleri', hinta: 4, lkm: 30 },
    { id: 6, nimi: 'selleri', hinta: 6, lkm: 70 },
    { id: 7, nimi: 'maito', hinta: 6, lkm: 70 },
]

describe('executeStringFunction()', () => {
    const functionDetailList = [
        {
            type: 'stringFunction',
            name: 'LENGTH',
            value: 'LENGTH(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'stringFunction',
            name: 'LENGTH',
            value: 'LENGTH(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'stringFunction',
            name: 'LENGTH',
            value: 'LENGTH(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'stringFunction',
            name: 'LENGTH',
            value: "LENGTH('string')",
            param: { type: 'string', value: 'string' },
        },
        {
            type: 'stringFunction',
            name: 'LENGTH',
            value: 'LENGTH(123)',
            param: { type: 'integer', value: 123 },
        },
    ]

    const row = rows[1]

    test(`returns the expected result for: ${functionDetailList[0].value}`, () => {
        expect(executeStringFunction(functionDetailList[0], row)).toBe(8)
    })

    test(`returns the expected result for: ${functionDetailList[1].value}`, () => {
        expect(executeStringFunction(functionDetailList[1], row)).toBe(1)
    })

    test(`returns the expected result for: ${functionDetailList[2].value}`, () => {
        expect(executeStringFunction(functionDetailList[2], row)).toBe(2)
    })

    test(`returns the expected result for: ${functionDetailList[3].value}`, () => {
        expect(executeStringFunction(functionDetailList[3], row)).toBe(6)
    })

    test(`returns the expected result for: ${functionDetailList[4].value}`, () => {
        expect(executeStringFunction(functionDetailList[4], row)).toBe(3)
    })
})

// Now expected values for AVG are the expected return values of lodash _.meanBy
describe('executeAggregateFunction()', () => {
    const functionDetailList = [
        {
            type: 'aggregateFunction',
            name: 'COUNT',
            value: 'COUNT(*)',
            param: { type: 'all', value: '*' },
        },
        {
            type: 'aggregateFunction',
            name: 'COUNT',
            value: 'COUNT(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'aggregateFunction',
            name: 'COUNT',
            value: 'COUNT(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'aggregateFunction',
            name: 'COUNT',
            value: 'COUNT(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'aggregateFunction',
            name: 'MAX',
            value: 'MAX(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'aggregateFunction',
            name: 'MAX',
            value: 'MAX(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'aggregateFunction',
            name: 'MAX',
            value: 'MAX(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'aggregateFunction',
            name: 'MAX',
            value: 'MAX(nonexistent)',
            param: { type: 'column', value: 'nonexistent' },
        },
        {
            type: 'aggregateFunction',
            name: 'MIN',
            value: 'MIN(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'aggregateFunction',
            name: 'MIN',
            value: 'MIN(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'aggregateFunction',
            name: 'MIN',
            value: 'MIN(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'aggregateFunction',
            name: 'MIN',
            value: 'MIN(nonexistent)',
            param: { type: 'column', value: 'nonexistent' },
        },
        {
            type: 'aggregateFunction',
            name: 'SUM',
            value: 'SUM(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'aggregateFunction',
            name: 'SUM',
            value: 'SUM(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'aggregateFunction',
            name: 'SUM',
            value: 'SUM(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'aggregateFunction',
            name: 'SUM',
            value: 'SUM(nonexistent)',
            param: { type: 'column', value: 'nonexistent' },
        },
        {
            type: 'aggregateFunction',
            name: 'AVG',
            value: 'AVG(hinta)',
            param: { type: 'column', value: 'hinta' },
        },
        {
            type: 'aggregateFunction',
            name: 'AVG',
            value: 'AVG(lkm)',
            param: { type: 'column', value: 'lkm' },
        },
        {
            type: 'aggregateFunction',
            name: 'AVG',
            value: 'AVG(nimi)',
            param: { type: 'column', value: 'nimi' },
        },
        {
            type: 'aggregateFunction',
            name: 'AVG',
            value: 'AVG(nonexistent)',
            param: { type: 'column', value: 'nonexistent' },
        },
    ]

    test(`returns the expected result with ${functionDetailList[0].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[0], rows)).toBe(7)
    })

    test(`returns the expected result with ${functionDetailList[1].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[1], rows)).toBe(7)
    })

    test(`returns the expected result with ${functionDetailList[2].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[2], rows)).toBe(7)
    })

    test(`returns the expected result with ${functionDetailList[3].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[3], rows)).toBe(7)
    })

    test(`returns the expected result with ${functionDetailList[4].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[4], rows)).toBe(8)
    })

    test(`returns the expected result with ${functionDetailList[5].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[5], rows)).toBe(70)
    })

    test(`returns the expected result with ${functionDetailList[6].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[6], rows)).toBe(
            'selleri'
        )
    })

    test(`returns the expected error object with ${functionDetailList[7].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[7], rows)).toEqual({
            error: 'Parameter given to MAX does not match any existing column',
        })
    })

    test(`returns the expected result with ${functionDetailList[8].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[8], rows)).toBe(4)
    })

    test(`returns the expected result with ${functionDetailList[9].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[9], rows)).toBe(20)
    })

    test(`returns the expected result with ${functionDetailList[10].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[10], rows)).toBe(
            'lanttu'
        )
    })

    test(`returns the expected error object with ${functionDetailList[11].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[11], rows)).toEqual({
            error: 'Parameter given to MIN does not match any existing column',
        })
    })

    test(`returns the expected result with ${functionDetailList[12].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[12], rows)).toBe(40)
    })

    test(`returns the expected result with ${functionDetailList[13].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[13], rows)).toBe(290)
    })

    test(`returns the expected result with ${functionDetailList[14].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[14], rows)).toBe(0)
    })

    test(`returns the expected error object with ${functionDetailList[15].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[15], rows)).toEqual({
            error: 'Parameter given to SUM does not match any existing column',
        })
    })

    test(`returns the expected result with ${functionDetailList[16].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[16], rows)).toBe(
            5.714285714285714
        )
    })

    test(`returns the expected result with ${functionDetailList[17].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[17], rows)).toBe(
            41.42857142857143
        )
    })

    test(`returns the expected result with ${functionDetailList[18].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[18], rows)).toBe(0)
    })

    test(`returns the expected error object with ${functionDetailList[19].value}`, () => {
        expect(executeAggregateFunction(functionDetailList[19], rows)).toEqual({
            error: 'Parameter given to AVG does not match any existing column',
        })
    })
})
