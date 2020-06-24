const selectParser = require('../../../commandParsers/selectParser')
const commandService = require('../../../services/commandService')
const splitCommandIntoArray = require('../../../commandParsers/parserTools/splitCommandIntoArray')

describe.each(['SELEC * FROM Taulu;', 'SELECT FROM Taulu;'])(
    'Query beginning with misspelled SELECT *',
    (wrongCommand) => {
        describe(wrongCommand, () => {
            const command = splitCommandIntoArray(wrongCommand)

            test('fails validation after parsed to command object', () => {
                expect(() => selectParser.parseCommand(command)).toThrowError()
            })
        })
    }
)

describe.each([
    'SELECT * FROM Taulu76;',
    'SELECT    * FROM        Taulu;',
    'seLEcT * FrOm Taulu;',
    'SELECT * FROM Taulun_nimi;',
])('Valid SELECT * -query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = selectParser.parseCommand(command)

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('name')
            expect(parsedCommand).toHaveProperty('from')
            expect(parsedCommand).toHaveProperty('tableName')
            expect(parsedCommand).toHaveProperty('finalSemicolon')
        })
    })
})

describe.each([
    'SELECT * FRM Taulu;',
    'SELECT * FROM Tau&lu;',
    'SELECT * FROM Taulu:',
    'SELECT * FROM Taulu',
    'SELECT * FROM',
    // 'SELECT * FROM Taulu)a;', FIXME: Requires check for extra input after command
    // 'SELECT * FROM Taulu additional ;', FIXME: Requires check for extra input after command
])('Invalid SELECT * -query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => selectParser.parseCommand(command)).toThrowError()
        })
    })
})

describe.each([
    'SELECT * FROM Tuotteet WHERE price=7;',
    'SELECT * FROM Tuotteet WhEre price=7;',
    'SELECT * FROM Tuotteet WHERE price = 7;',
    'SELECT * FROM Tuotteet WHERE price =7;',
    "SELECT * FROM Tuotteet WHERE name = ' test ';",
    "SELECT * FROM Tuotteet WHERE name=' test';",
    "SELECT * FROM Tuotteet WHERE name='test ';",
    "SELECT * FROM Tuotteet WHERE name='test';",
])('Valid SELECT * FROM ... WHERE ...-query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is recognised as a command', () => {
            expect(commandService.parseCommand(command)).toBeTruthy()
        })

        test('is parsed and validated succesfully', () => {
            const parsedCommand = selectParser.parseCommand(command)

            expect(parsedCommand).toBeDefined()
            expect(parsedCommand).toHaveProperty('where')
        })
    })
})

describe.each([
    'SELECT * FROM Tuotteet WHERE  = 7;',
    'SELECT * FROM Tuotteet WHERE price 7;',
    'SELECT * FROM Tuotteet WHERE price7;',
    "SELECT * FROM Tuotteet WHERE name = ' test '",
    "SELECT * FROM Tuotteet WHERE name='';",
    "SELECT * FROM Tuotteet WHERE name name='test';",
    "SELECT * FROM Tuotteet WHERE name='test' additional;",
    // "SELECT * FROM Tuotteet WHERE name='test' ';", FIXME: Additional after where part
])('Invalid SELECT * FROM ... WHERE ...-query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('fails validation after parsed to command object', () => {
            expect(() => selectParser.parseCommand(command)).toThrowError()
        })
    })
})

/* FIXME: Requires check for extra input after command
describe.each([
    'SELECT * FROM Tuotteet WHEE price=7;',
    'SELECT * FROM Tuotteet  price=7;',
])(
    'SELECT * FROM ... WHERE ...-query with misspelled or missing WHERE',
    (validCommand) => {
        describe(validCommand, () => {
            const command = splitCommandIntoArray(validCommand)

            test('is recognised as a command', () => {
                expect(commandService.parseCommand(command)).toBeTruthy()
            })

            test('fails validation after parsed to command object', () => {
                const parsedCommand = selectParser.parseCommand(command)

                expect(parsedCommand).toBeDefined()
                expect(parsedCommand).not.toHaveProperty('where')
                expect(parsedCommand).toThrow()
            })
        })
    }
)
*/
