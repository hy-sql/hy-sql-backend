const { parseCommand } = require('../../../commandParsers/selectParser')
const splitCommandIntoArray = require('../../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    'SELEC * FROM Taulu;',
    'SELECT FROM Taulu ORDER;',
    'SELECT FROM Taulu BY ORDER;',
])('SELECT * query not containing ORDER BY', (wrongCommand) => {
    describe(wrongCommand, () => {
        const command = splitCommandIntoArray(wrongCommand)

        test('does not contain orderBy field', () => {
            expect(() => parseCommand(command)).toThrowError()
        })
    })
})

describe.each([
    'SELECT * FROM Taulu76 order by column ;',
    'SELECT    * FROM        Taulu ORDER BY columnS;',
    'seLEcT * FrOm Taulu   order BY column asc;',
    'SELECT * FROM Taulun_nimi ORder BY column Asc;',
    'SELECT * FROM Tuotteet ORDER BY hinta desc;',
    'SELECT * FROM Tuotteet ORDER BY hinta   dESc  ;',
])('Valid SELECT * ORDER BY -query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = parseCommand(command)

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('name')
            expect(parsedCommand).toHaveProperty('from')
            expect(parsedCommand).toHaveProperty('tableName')
            expect(parsedCommand).toHaveProperty('finalSemicolon')
            expect(parsedCommand).toHaveProperty('orderBy')
        })
    })
})

describe.each([
    'SELECT * FROM Taulu order by where where;',
    'SELECT * FROM Taulu order by asc hi;',
    'SELECT * FROM Taulu order by asc hi:',
    'SELECT * FROM Taulu order asc by;',
    'SELECT * FROM Taulu by order',
    'SELECT * FROM Taulu order by)a;',
    'SELECT nimi, hinta FROM Tuotteet ORDER BY DESC;',
    'SELECT nimi, hinta FROM Tuotteet ORDER BY ASC;',
])('Invalid SELECT * -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => parseCommand(command)).toThrowError()
        })
    })
})
