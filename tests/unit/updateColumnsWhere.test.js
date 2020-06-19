const commandService = require('../../services/commandService')
const updateParser = require('../../commandParsers/updateParser')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris';",
    "update tuotteet SET hinta=6, nimi='nauris' WHERE nimi='retiisi';",
    "update tuotteet set hinta=6 where nimi='ananas';",
    "uPdAtE Tuotteet sEt nimi='nauris' WheRe hinta=6;",
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris' AND hinta=6;",
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris' OR hinta=6;",
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris' OR (nimi='nauris' AND hinta=6);",
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

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('name')
            expect(parsedCommand).toHaveProperty('tableName')
            expect(parsedCommand).toHaveProperty('set')
            expect(parsedCommand).toHaveProperty('finalSemicolon')
            expect(parsedCommand).toHaveProperty('where')
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
        const fullCommandAsStringArray = splitCommandIntoArray(command)

        test('fails validation after parsed to command object', () => {
            expect(() => {
                updateParser.parseCommand(fullCommandAsStringArray)
            }).toThrow()
        })
    })
})
