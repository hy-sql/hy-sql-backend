const deleteCommand = require('../commands/deleteCommand')
const commandService = require('../services/commandService')
const cleanCommand = require('../utils/cleanCommand')

describe.each(['DEL FROM Taulu;'])(
    'Query beginning with misspelled DELETE',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = cleanCommand(wrongCommand)

            test('does not pass validation', () => {
                expect(deleteCommand.parseCommand(command).error).toBeDefined()
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
        const command = cleanCommand(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = deleteCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('name')
            expect(parsedCommand.value).toHaveProperty('from')
            expect(parsedCommand.value).toHaveProperty('tableName')
            expect(parsedCommand.value).toHaveProperty('finalSemicolon')

            expect(parsedCommand.error).toBeUndefined()
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
        const command = cleanCommand(invalidCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            expect(deleteCommand.parseCommand(command).error).toBeDefined()
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
        const command = cleanCommand(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = deleteCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.error).toBeUndefined()
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
    "DELETE FROM Tuotteet WHERE name='test' ';",
])('Invalid DELETE FROM ... WHERE ...-query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = cleanCommand(invalidCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = deleteCommand.parseCommand(command)

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('where')
            expect(parsedCommand.error).toBeDefined()
        })
    })
})