const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')
const SQLError = require('../../models/SQLError')

describe('selectFromTable()', () => {
    test('returns error when table does not exist', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const command = {
            name: 'SELECT',
            tableName: 'products',
        }

        expect(() => stateService.selectFrom(command)).toThrowError(
            new SQLError('No such table products')
        )
    })

    test('returns column nimi for SELECT nimi FROM Tuotteet;', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectCommand = 'SELECT nimi from Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)
        expect(result.rows.length).toBe(1)
        expect(result.rows[0]['nimi']).toBe('tuote')
    })

    test('returns error when queried column does not exist', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectCommand = 'SELECT invalid from Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        expect(() => stateService.selectFrom(parsedCommand)).toThrowError(
            new SQLError('no such column invalid')
        )
    })
})

describe('selectFrom() with command.where', () => {
    let stateService

    beforeEach(() => {
        const state = new State(new Map())
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (testituote, 20);',
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    test('returns filtered rows when where is defined', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta=10;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)

        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when command.where contains >', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta>5;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)

        expect(result.rows.length).toBe(2)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains >=', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta>=20;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)

        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains <', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta<20;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)

        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains <=', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta<=10;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectFrom(parsedCommand)

        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })
})

describe('selectFrom() with ORDER BY -command', () => {
    test('returns rows from table in ascending order', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const expectedRows = [
            {
                nimi: 'selleri',
                hinta: 1,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
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
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',1);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',6);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY hinta;'

        const splitSelectAllOrderByCommand = splitCommandIntoArray(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(parsedSelectAllOrderByCommand)

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFrom() with ORDER BY DESC -command (numbers)', () => {
    test('returns rows from table in ascending order', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const expectedRows = [
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 1,
            },
        ]

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu',8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',1);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',6);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY hinta DESC;'

        const splitSelectAllOrderByCommand = splitCommandIntoArray(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(parsedSelectAllOrderByCommand)

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFrom() with ORDER BY ASC -command', () => {
    test('returns rows from table in ascending order', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const expectedRows = [
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 1,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
        ]

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu',8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',1);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',6);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi ASC;'

        const splitSelectAllOrderByCommand = splitCommandIntoArray(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(parsedSelectAllOrderByCommand)

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFrom() with ORDER BY DESC -command (text)', () => {
    test('returns rows from table in ascending order', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const expectedRows = [
            {
                nimi: 'selleri',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 1,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
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
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',1);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',6);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi DESC;'

        const splitSelectAllOrderByCommand = splitCommandIntoArray(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(parsedSelectAllOrderByCommand)

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFrom() with command.where and command.orderBy', () => {
    let stateService

    beforeEach(() => {
        const state = new State(new Map())
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
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    test('returns filtered rows when where and orderBy is defined', () => {
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
            "SELECT id, nimi, hinta from Tuotteet WHERE nimi='olut' ORDER BY hinta ASC;"

        const commandArray = splitCommandIntoArray(selectCommand)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)

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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta>5 ORDER BY hinta ASC;'

        const commandArray = splitCommandIntoArray(selectCommand)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)

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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta>=5 ORDER BY hinta ASC;'

        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)

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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta<5 ORDER BY hinta DESC;'

        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta<=3 ORDER BY hinta DESC;'

        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFrom() with DISTINCT keyword', () => {
    let stateService

    beforeEach(() => {
        const state = new State(new Map())
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',3);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',3);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',2);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('olut',2);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    test('and one column returns correct columns', () => {
        const expectedRows = [
            {
                nimi: 'retiisi',
            },
            {
                nimi: 'nauris',
            },
            {
                nimi: 'selleri',
            },
            {
                nimi: 'olut',
            },
        ]
        const selectCommand = 'SELECT DISTINCT nimi FROM Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('and two columns returns correct columns', () => {
        const expectedRows = [
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                nimi: 'nauris',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
            {
                nimi: 'olut',
                hinta: 3,
            },
            {
                nimi: 'olut',
                hinta: 2,
            },
        ]

        const selectCommand = 'SELECT DISTINCT nimi, hinta FROM Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('and WHERE keyword returns correct columns', () => {
        const expectedRows = [
            {
                hinta: 3,
            },
            {
                hinta: 2,
            },
        ]
        const selectCommand =
            "SELECT DISTINCT hinta FROM Tuotteet WHERE nimi='olut';"
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('and WHERE + ORDER BY keyword returns correct columns', () => {
        const expectedRows = [
            {
                hinta: 2,
            },
            {
                hinta: 3,
            },
        ]

        const selectCommand =
            "SELECT DISTINCT hinta FROM Tuotteet WHERE nimi='olut' ORDER BY hinta ASC;"
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('invalid DISTINCT returns error', () => {
        const selectCommand =
            "SELECT DISTIn hinta FROM Tuotteet WHERE nimi='olut' ORDER BY hinta ASC;"
        const commandArray = splitCommandIntoArray(selectCommand)

        expect(() => commandService.parseCommand(commandArray)).toThrowError(
            new SQLError('fields must be split by comma (,)')
        )
    })

    test('length()-function returns correct columns', () => {
        const expectedRows = [
            {
                'length(nimi)': 7,
            },
            {
                'length(nimi)': 6,
            },
            {
                'length(nimi)': 4,
            },
        ]

        const selectCommand = 'SELECT DISTINCT length(nimi) FROM Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('aritmetic function returns correct columns', () => {
        const expectedRows = [
            {
                'hinta+1': 8,
            },
            {
                'hinta+1': 6,
            },
            {
                'hinta+1': 5,
            },
            {
                'hinta+1': 4,
            },
            {
                'hinta+1': 3,
            },
        ]

        const selectCommand = 'SELECT DISTINCT hinta+1 FROM Tuotteet;'
        const commandArray = splitCommandIntoArray(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })
})
