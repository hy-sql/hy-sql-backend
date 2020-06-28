const deleteParser = require('../../commandParsers/deleteParser')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each(['DEL FROM Taulu;'])(
    'Query beginning with misspelled DELETE',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = splitCommandIntoArray(wrongCommand)

            test('fails validation after parsed to command object', () => {
                expect(() => {
                    deleteParser.parseCommand(command)
                }).toThrowError()
            })
        })
    }
)

describe.each([
    'DELETE FROM Taulu8;',
    'DELETE FROM Taulu_8;',
    'dElEtE FrOm Taulu;',
    'DELETE    FROM      Taulu    ;',
])('Valid DELETE-query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = deleteParser.parseCommand(command)

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('name')
            expect(parsedCommand).toHaveProperty('from')
            expect(parsedCommand).toHaveProperty('tableName')
            expect(parsedCommand).toHaveProperty('finalSemicolon')
        })
    })
})

describe.each([
    'DELETE FRM Taulu;',
    'DELETE FROM Tau&lu;',
    'DELETE FROM Taulu:',
    'DELETE FROM Taulu',
    'DELETE FROM',
    'DELETE FROM Taulu)a;',
    'DELETE FROM Taulu additonal ;',
])('Invalid DELETE-query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => {
                deleteParser.parseCommand(command)
            }).toThrowError()
        })
    })
})

describe.each([
    'DELETE FROM Tuotteet WHERE price=7;',
    'DELETE FROM Tuotteet WhEre price=7;',
    'DELETE FROM Tuotteet WHERE price = 7;',
    'DELETE FROM Tuotteet WHERE price =7;',
    "DELETE FROM Tuotteet WHERE name = ' test ';",
    "DELETE FROM Tuotteet WHERE name=' test';",
    "DELETE FROM Tuotteet WHERE name='test ';",
    "DELETE FROM Tuotteet WHERE name='test';",
])('Valid DELETE FROM ... WHERE ...-query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = deleteParser.parseCommand(command)

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('where')
        })
    })
})

describe.each([
    'DELETE FROM Tuotteet WHERE  = 7;',
    'DELETE FROM Tuotteet WHERE price 7;',
    'DELETE FROM Tuotteet WHERE price7;',
    "DELETE FROM Tuotteet WHERE name = ' test '",
    "DELETE FROM Tuotteet WHERE name='';",
    "DELETE FROM Tuotteet WHERE price name='test';",
    "DELETE FROM Tuotteet WHERE name='test' additional;",
    // "DELETE FROM Tuotteet WHERE name='test' ';", FIX ME: requires checking for additional at end of DELETE...WHERE-query
])('Invalid DELETE FROM ... WHERE ...-query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => {
                deleteParser.parseCommand(command)
            }).toThrowError()
        })
    })
})
