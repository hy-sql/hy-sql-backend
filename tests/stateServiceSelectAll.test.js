const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

describe('selectAllFromTable()', () => {
    test('returns error when table does not exist', () => {
        const initTables = []
        const state = new State(initTables)
        const stateService = new StateService(state)
        const selectCommand = {
            name: 'SELECT *',
            tableName: 'Tuotteet',
        }
        const result = stateService.selectAllFromTable(selectCommand)
        expect(result.error).toBe('No such table Tuotteet')
    })

    test('returns all the rows from table', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)
        const createCommand = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'hinta', type: 'INTEGER', primaryKey: false },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }
        stateService.createTable(createCommand)

        const insertCommand =
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);'
        const splitCommand = insertCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(splitCommand)

        stateService.insertIntoTable(parsedCommand.value)
        const selectCommand = {
            name: 'SELECT *',
            tableName: 'Tuotteet',
        }
        const result = stateService.selectAllFromTable(selectCommand)
        expect(result.rows.length).toBe(1)
    })
})

describe('selectAllFromTable() with ORDER BY -command', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const expectedRows = [
            {
                id: 3,
                nimi: 'nauris',
                hinta: 4,
            },
            {
                id: 5,
                nimi: 'selleri',
                hinta: 4,
            },
            {
                id: 2,
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                id: 1,
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                id: 4,
                nimi: 'lanttu',
                hinta: 8,
            },
        ]

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu',8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
        ]

        const splitCommandArray = commands.map((input) =>
            input
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectAllOrderByCommand = 'SELECT * FROM Tuotteet ORDER BY hinta;'

        const splitSelectAllOrderByCommand = selectAllOrderByCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.selectAllFromTable(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectAllFromTable() with command.where', () => {
    let stateService

    beforeEach(() => {
        const initArray = []
        const state = new State(initArray)
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (testituote, 20);',
        ]

        const splitCommandArray = commands.map((input) =>
            input
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    test('returns filtered rows when where is defined', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta=10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })

    test('returns filtered rows when where-command contains >', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta>10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta>10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
    })

    test('returns filtered rows when where-command contains >=', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta>=20;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta>=20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
    })

    test('returns filtered rows when where-command contains <', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta<20;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta<20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })

    test('returns filtered rows when where-command contains <=', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta<=10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta<=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })
})

describe('selectAllFromTable() with command.where and command.orderBy', () => {
    let stateService

    beforeEach(() => {
        const initArray = []
        const state = new State(initArray)
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu',8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',6);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',3);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',2);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',5);",
        ]

        const splitCommandArray = commands.map((input) =>
            input
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    test('returns filtered rows when where is defined', () => {
        const expectedRows = [
            {
                id: 9,
                nimi: 'olut',
                hinta: 2,
            },
            {
                id: 8,
                nimi: 'olut',
                hinta: 3,
            },
            {
                id: 7,
                nimi: 'olut',
                hinta: 4,
            },
            {
                id: 10,
                nimi: 'olut',
                hinta: 5,
            },
        ]

        const selectCommand =
            "SELECT * from Tuotteet WHERE nimi='olut' ORDER BY ASC;"
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test('returns filtered rows when command.where contains >', () => {
        const expectedRows = [
            {
                id: 6,
                nimi: 'selleri',
                hinta: 6,
            },
            {
                id: 1,
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                id: 4,
                nimi: 'lanttu',
                hinta: 8,
            },
        ]
        const selectCommand =
            'SELECT * FROM Tuotteet WHERE hinta>5 ORDER BY ASC;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)

        console.log(result.rows)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns filtered rows when where-command contains >=', () => {
        const expectedRows = [
            {
                id: 2,
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                id: 10,
                nimi: 'olut',
                hinta: 5,
            },
            {
                id: 6,
                nimi: 'selleri',
                hinta: 6,
            },
            {
                id: 1,
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                id: 4,
                nimi: 'lanttu',
                hinta: 8,
            },
        ]

        const selectCommand =
            'SELECT * FROM Tuotteet WHERE hinta>=5 ORDER BY ASC;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test('returns filtered rows when where-command contains <', () => {
        const expectedRows = [
            {
                id: 3,
                nimi: 'nauris',
                hinta: 4,
            },
            {
                id: 5,
                nimi: 'selleri',
                hinta: 4,
            },
            {
                id: 7,
                nimi: 'olut',
                hinta: 4,
            },
            {
                id: 8,
                nimi: 'olut',
                hinta: 3,
            },
            {
                id: 9,
                nimi: 'olut',
                hinta: 2,
            },
        ]

        const selectCommand =
            'SELECT * FROM Tuotteet WHERE hinta<5 ORDER BY hinta DESC;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns filtered rows when where-command contains <=', () => {
        const expectedRows = [
            {
                id: 8,
                nimi: 'olut',
                hinta: 3,
            },
            {
                id: 9,
                nimi: 'olut',
                hinta: 2,
            },
        ]

        const selectCommand =
            'SELECT * FROM Tuotteet WHERE hinta<=3 ORDER BY hinta DESC;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })
})
