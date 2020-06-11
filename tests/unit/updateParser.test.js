const commandService = require('../../services/commandService')
const updateParser = require('../../commandParsers/updateAdvancedParser')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'UPDATE Tuotteet SET hinta=6;',
    "update tuotteet SET hinta=6, nimi='nauris';",
    'update tuotteet set hinta=6;',
    "uPdAtE Tuotteet sEt nimi='nauris';",
])('Valid UPDATE command testing', (command) => {
    describe(command, () => {
        const fullCommandAsStringArray = splitCommandIntoArray(command)

        test('is recognized as UPDATE command', () => {
            const result = commandService.parseCommand(fullCommandAsStringArray)

            expect(result).toBeTruthy()
        })

        test('is parsed and validated successfully', () => {
            const parsedCommand = updateParser.parseCommand(
                fullCommandAsStringArray
            )

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.value).toHaveProperty('name')
            expect(parsedCommand.value).toHaveProperty('tableName')
            expect(parsedCommand.value).toHaveProperty('set')
            expect(parsedCommand.value).toHaveProperty('finalSemicolon')

            expect(parsedCommand.error).not.toBeDefined()
        })
    })
})

describe.each([
    'UPDATE Tuotteet;',
    'update tuotteet 123 set;',
    'UPDATE tuotteet set 123;',
    "update tuotteet set 'nauris';",
    "update tuotteet 'nauris';",
    "update tuotteet 'nauris'",
    "update tuotteet set nimi: 'nauris';",
])('Invalid UPDATE command testing', (command) => {
    describe(command, () => {
        const fullCommandAsStringArray = splitCommandIntoArray(command)

        test('is recognized as UPDATE command', () => {
            const result = commandService.parseCommand(fullCommandAsStringArray)

            expect(result).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = commandService.parseCommand(
                fullCommandAsStringArray
            )

            expect(parsedCommand.error).toBeDefined()
        })
    })
})
