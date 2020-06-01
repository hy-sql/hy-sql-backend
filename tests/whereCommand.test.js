const {
    queryContainsWhereKeyword,
    parseWhereToCommandObject,
} = require('../commands/whereCommand')

const WhereSchema = require('../models/WhereSchema')

describe.each([
    'WHERE price=7;',
    'WhEre price=7;',
    'WHERE price = 7;',
    'WHERE price =7;',
    "WHERE name='test';",
    "WHERE name = ' test ';",
    "WHERE name=' test';",
    "WHERE name='test ';",
    "WHERE name='test' ORDER BY",
])('Valid WHERE-part of a query', (validCommand) => {
    describe(validCommand, () => {
        const command = validCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        test('is recognised as a Where-command', () => {
            expect(queryContainsWhereKeyword(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = WhereSchema.validate(
                parseWhereToCommandObject(command)
            )
            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('columnName')
            expect(parsedCommand.value).toHaveProperty('sign')
            expect(parsedCommand.value).toHaveProperty('valueType')
            expect(parsedCommand.value).toHaveProperty('value')
            expect(parsedCommand.value).toHaveProperty('indexCounter')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'WHERE  = 7;',
    'WHERE price 7;',
    'WHERE price7;',
    "WHERE name='';",
    "WHERE name name='test ';",
])('Invalid WHERE-part of a query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = invalidCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        test('is recognised as a Where-command', () => {
            expect(queryContainsWhereKeyword(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = WhereSchema.validate(
                parseWhereToCommandObject(command)
            )
            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.error).toBeDefined()
        })
    })
})

describe.each(['WHEE price=7;', 'price=7;'])(
    'Query part with misspelled or missing WHERE',
    (validCommand) => {
        describe(validCommand, () => {
            const command = validCommand
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)

            test('is not recognised as a Where-command', () => {
                expect(queryContainsWhereKeyword(command)).toBeFalsy()
            })

            test('fails validation after parsed to command object', () => {
                const parsedCommand = WhereSchema.validate(
                    parseWhereToCommandObject(command)
                )
                expect(parsedCommand.value).toBeDefined()
                expect(parsedCommand.error).toBeDefined()
            })
        })
    }
)
