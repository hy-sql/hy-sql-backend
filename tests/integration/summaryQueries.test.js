const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe('large queries', () => {
    let stateService

    beforeEach(() => {
        const state = new State(new Map())
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, tuote_id INTEGER, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (1, 'porkkana', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (1, 'banaani', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (2, 'banaani', 4);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (2, 'retiisi', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (5, 'nauris', 2);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (6, 'bucket', 0);",
        ]

        // const testCommands = [
        //     'SELECT tuote_id, COUNT(*) FROM Tuotteet WHERE hinta >= 3 GROUP BY tuote_id ORDER BY tuote_id;',
        //     'SELECT projekti_id, COUNT(*) FROM Tehtavat WHERE tarkeys >= 3 GROUP BY projekti_id HAVING COUNT(*) >= 2 ORDER BY projekti_id;',
        //     "SELECT nimi, hinta, 5*hinta+3, LENGTH(nimi) FROM Tuotteet WHERE hinta=LENGTH(nimi) OR (hinta+1=5 AND nimi<>'banaani') OR (hinta=2*hinta) ORDER BY LENGTH(hinta), hinta;",
        // ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    test('', () => {
        const expectedRows = [
            {
                tuote_id: 1,
                'COUNT(*)': 2,
            },
            {
                tuote_id: 2,
                'COUNT(*)': 2,
            },
        ]
        const selectParser =
            'SELECT tuote_id, COUNT(*) FROM Tuotteet WHERE hinta >= 3 GROUP BY tuote_id ORDER BY tuote_id;'
        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })
})
