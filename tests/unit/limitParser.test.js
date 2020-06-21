const { parseLimit } = require('../../commandParsers/limitParser')
const {
    queryContainsLimitKeyword,
} = require('../../commandParsers/parserTools/queryContains')
const { LimitSchema } = require('../../schemas/LimitSchema')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 20;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 + 2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2*5+3;',
])('Valid query containing LIMIT', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a LIMIT keyword', () => {
            expect(queryContainsLimitKeyword(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = LimitSchema.validate(parseLimit(command))

            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('field')
            expect(parsedCommand.value).not.toHaveProperty('offset')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 30;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 2 + 2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 2*5+3;',
])('Valid query containing LIMIT OFFSET', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is parsed and validated succesfully', () => {
            const parsedCommand = LimitSchema.validate(parseLimit(command))

            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('field')
            expect(parsedCommand.value).toHaveProperty('offset')
            expect(parsedCommand.value.offset).toHaveProperty('keyword')
            expect(parsedCommand.value.offset).toHaveProperty('field')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'SELECT nimi, hinta FROM Tuotteet LIMIT column;',
    "SELECT nimi, hinta FROM Tuotteet LIMIT '24';",
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2a+2;',
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET column;',
    "SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET '24';",
    'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 2a*2;',
])('Invalid query containing LIMIT or LIMIT OFFSET', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('is recognised to contain LIMIT keyword', () => {
            expect(queryContainsLimitKeyword(command)).toBeTruthy()
        })

        test('fails validation after parsing', () => {
            const parsedCommand = LimitSchema.validate(parseLimit(command))

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.error).toBeDefined()
        })
    })
})
