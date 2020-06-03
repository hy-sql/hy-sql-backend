const commandService = require('../services/commandService')
const updateCommand = require('../commands/updateCommand')

describe.each([
    'UPDATE Tuotteet SET hinta=6;',
    "update tuotteet SET hinta=6, nimi='nauris';",
    'update tuotteet set hinta=6;',
    "uPdAtE Tuotteet sEt nimi='nauris';",
])('Valid command testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test(`valid command ${command} is recognized and true returned`, () => {
        const result = commandService.parseCommand(fullCommandAsStringList)
        // console.log(result)

        expect(result).toBeTruthy()
    })

    test(`valid command ${command} is parsed and validated successfully`, () => {
        const parsedCommand = updateCommand.parseCommand(
            fullCommandAsStringList
        )

        expect(parsedCommand.value).toBeDefined()
        expect(parsedCommand.value).toHaveProperty('name')
        expect(parsedCommand.value).toHaveProperty('tableName')
        expect(parsedCommand.value).toHaveProperty('set')
        expect(parsedCommand.value).toHaveProperty('finalSemicolon')

        expect(parsedCommand.error).not.toBeDefined()
    })
})

describe.each([
    'UPDATE Tuotteet;',
    'update tuotteet 123 set;',
    'UPDATE tuotteet set 123;',
    "update tuotteet set 'nauris';",
    "update tuotteet 'nauris';",
    "update tuotteet 'nauris'",
])('Invalid command testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test(`invalid command ${command} is parsed but error returned`, () => {
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringList
        )
        // console.log(parsedCommand)

        expect(parsedCommand.error).toBeDefined()
    })
})
