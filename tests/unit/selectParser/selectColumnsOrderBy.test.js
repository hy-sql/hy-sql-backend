const selectParser = require('../../../commandParsers/selectParser')
const commandService = require('../../../services/commandService')
const splitCommandIntoArray = require('../../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    "SELECT id, nimi, hinta FROM Tuotteet WHERE this='that';",
    'SELECT nimi FROM Tuotteet WHERE hinta=5;',
    'select id, nimi, hinta from tuotteet where hinta<5;',
    'seLEct  id, NIMI, hintA FROM tuoTTeet WheRE hinta>5;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet   WHERE    hinta<=5;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet   WHERE    hinta>=5;',
    'select id, NIMI, hinta fROM                    Tuotteet;',
])('valid command SELECT ... FROM WHERE testing', (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)

    test('valid command is recognized and true returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringArray)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = selectParser.parseCommand(
            fullCommandAsStringArray
        )

        expect(parsedCommand.error).not.toBeDefined()
    })
})

describe.each([
    'SELECT id nimi, hinta FROM Tuotteet WHERE this=?;',
    'SELECT id,nimi,hinta FROM Tuotteet WHERE',
    'SELECT id,nimi,hinta FROM  where nothing;',
    '       seLEct FROM      tuoTTeet  WHERE WHERE;',
    "   selecT id nimi hinta FROM tuoTTeeT WHERE this='that' null;",
    'SeleCT id,nimi,hinta   Tuotteet WHERE this=that;',
])(
    'invalid command with the right name (SELECT) and WHERE keyword testing',
    (command) => {
        const fullCommandAsStringArray = splitCommandIntoArray(command)

        test('invalid command is parsed but validation fails', () => {
            expect(() =>
                selectParser.parseCommand(fullCommandAsStringArray)
            ).toThrowError()
        })
    }
)
