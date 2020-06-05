const commandService = require('../services/commandService')
const updateCommand = require('../commands/updateCommand')
const cleanCommand = require('../utils/cleanCommand')

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris';",
    "update tuotteet SET hinta=6, nimi='nauris' WHERE nimi='retiisi';",
    "update tuotteet set hinta=6 where nimi='ananas';",
    "uPdAtE Tuotteet sEt nimi='nauris' WheRe hinta=6;",
])('Valid UPDATE command testing', (command) => {
    describe(command, () => {
        const fullCommandAsStringList = cleanCommand(command)

        test('is recognized as UPDATE command', () => {
            const result = commandService.parseCommand(fullCommandAsStringList)

            expect(result).toBeTruthy()
        })

        test('is parsed and validated successfully', () => {
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
})

describe.each([
    'UPDATE Tuotteet where hinta=6;',
    'update tuotteet 123 set hinta=6 WHERE hinta=5;',
    'UPDATE tuotteet set 123 WHEre hinta=6;',
    "update tuotteet set 'nauris' where hinta=6;",
    "update tuotteet 'nauris' where hinta=6;",
    "update tuotteet 'nauris' where 6",
    "update tuotteet set nimi: 'nauris' WHERE hinta=6;",
])('Invalid UPDATE command testing', (command) => {
    describe(command, () => {
        const fullCommandAsStringList = cleanCommand(command)

        test('is recognized as UPDATE command', () => {
            const result = commandService.parseCommand(fullCommandAsStringList)

            expect(result).toBeTruthy()
        })

        test('fails validation after parsed to command object', () => {
            const parsedCommand = commandService.parseCommand(
                fullCommandAsStringList
            )

            expect(parsedCommand.error).toBeDefined()
        })
    })
})
