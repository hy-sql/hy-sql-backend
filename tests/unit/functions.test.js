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
})
