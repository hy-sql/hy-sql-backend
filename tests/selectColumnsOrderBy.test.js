const selectCommand = require('../commands/selectCommand')
const { SelectColumnsOrderBySchema } = require('../models/SelectSchema')
const commandService = require('../services/commandService')

describe.each([
    "SELECT id, nimi, hinta FROM Tuotteet WHERE this='that';",
    'SELECT nimi FROM Tuotteet WHERE hinta=5;',
    'select id, nimi, hinta from tuotteet where hinta<5;',
    'seLEct  id, NIMI, hintA FROM tuoTTeet WheRE hinta>5;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet   WHERE    hinta<=5;',
    '   selecT     id       , NIMI, hintA FRoM tuoTTeet   WHERE    hinta>=5;',
    'select id, NIMI, hinta fROM                    Tuotteet;',
])('valid command SELECT ... FROM WHERE testing', (command) => {
    const fullCommandAsStringList = command
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    test('valid command is recognized and true returned', () => {
        const result = commandService.parseCommand(fullCommandAsStringList)

        expect(result).toBeTruthy()
    })

    test('valid command is parsed and validated successfully', () => {
        const parsedCommand = selectCommand.parseCommand(
            fullCommandAsStringList
        )

        expect(parsedCommand.error).not.toBeDefined()
    })
})

describe.each([
    'SELECT id nimi, hinta FROM Tuotteet WHERE this=?;', //eka sarakkeiden pilkku puuttuu
    'SELECT id,nimi,hinta FROM Tuotteet WHERE', //puolipiste lopusta
    'SELECT id,nimi,hinta FROM  where nothing;', //taulu puuttuu
    '       seLEct FROM      tuoTTeet  WHERE WHERE;', //sarakkeet puuttuu kokonaan
    "   selecT id nimi hinta FROM tuoTTeeT WHERE this='that' null;", //sarakkeiden kaikki pilkut puuttuu
    'SeleCT id,nimi,hinta   Tuotteet WHERE this=that;', //FROM puuttuu
])(
    'invalid command with the right name (SELECT) and WHERE keyword testing',
    (command) => {
        const fullCommandAsStringList = command
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        test('invalid command is parsed but validation fails', () => {
            const parsedCommand = selectCommand.parseCommand(
                fullCommandAsStringList
            )

            const result = SelectColumnsOrderBySchema.validate(parsedCommand)

            expect(result.error).toBeDefined()
        })
    }
)
