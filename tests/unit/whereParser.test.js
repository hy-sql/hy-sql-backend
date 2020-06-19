const { parseWhere } = require('../../commandParsers/whereParser')
const {
    queryContainsWhereKeyword,
} = require('../../commandParsers/parserTools/queryContains')
const WhereSchema = require('../../schemas/WhereSchema')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'WHERE price=7',
    'WhEre price=7',
    'WHERE price = 7',
    'WHERE price =7',
    "WHERE name='test'",
    "WHERE name = ' test '",
    "WHERE name=' test'",
    "WHERE name='test '",
    "WHERE name='test' ORDER BY",
])('Valid WHERE-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised to contain a WHERE keyword', () => {
            expect(queryContainsWhereKeyword(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = WhereSchema.validate(parseWhere(command))

            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('conditions')
        })
    })
})

describe.each([
    'WHERE  = 7',
    'WHERE price 7',
    'WHERE price7',
    "WHERE name=''",
    "WHERE name name='test '",
])('Invalid WHERE-part of a query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('is recognised to contain a WHERE keyword', () => {
            expect(queryContainsWhereKeyword(command)).toBeTruthy()
        })

        test('fails validation after parsing', () => {
            const parsedCommand = WhereSchema.validate(parseWhere(command))

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.error).toBeDefined()
        })
    })
})

describe.each(['WHEE price=7', 'price=7'])(
    'Query part with misspelled or missing WHERE',
    (validCommand) => {
        describe(validCommand, () => {
            const command = splitCommandIntoArray(validCommand)

            test('is recognised to contain a WHERE keyword', () => {
                expect(queryContainsWhereKeyword(command)).toBeFalsy()
            })

            test('fails validation after parsing', () => {
                const parsedCommand = WhereSchema.validate(parseWhere(command))
                expect(parsedCommand.value).toBeDefined()
                expect(parsedCommand.error).toBeDefined()
            })
        })
    }
)
