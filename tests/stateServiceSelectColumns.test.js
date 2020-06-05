const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')
const cleanCommand = require('../utils/cleanCommand')

describe('selectFromTable()', () => {
    test('returns error when table does not exist', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const command = {
            name: 'SELECT',
            tableName: 'products',
        }

        const result = stateService.selectColumnsFromTable(command)
        expect(result.error).toBe('No such table products')
    })

    test('returns column nimi for SELECT nimi FROM Tuotteet;', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
        ]

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectCommand = 'SELECT nimi from Tuotteet;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet -query was executed successfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0]['nimi']).toBe('tuote')
    })
})

describe('selectColumnsFromTable() with command.where', () => {
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    test('returns filtered rows when where is defined', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta=10;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet WHERE hinta=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when command.where contains >', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta>5;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet WHERE hinta>5 -query executed succesfully'
        )
        expect(result.rows.length).toBe(2)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains >=', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta>=20;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet WHERE hinta>=20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains <', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta<20;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet WHERE hinta<20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })

    test('returns filtered rows when where-command contains <=', () => {
        const selectCommand = 'SELECT nimi from Tuotteet WHERE hinta<=10;'
        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet WHERE hinta<=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
        expect(result.rows[0].hinta).toBe(undefined)
    })
})

describe('selectColumnsFromTable() with ORDER BY -command', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY hinta;'

        const splitSelectAllOrderByCommand = cleanCommand(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectColumnsFromTable() with ORDER BY DESC -command (numbers)', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY hinta DESC;'

        const splitSelectAllOrderByCommand = cleanCommand(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectColumnsFromTable() with ORDER BY ASC -command', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi ASC;'

        const splitSelectAllOrderByCommand = cleanCommand(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectColumnsFromTable() with ORDER BY DESC -command (text)', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))

        const selectAllOrderByCommand =
            'SELECT nimi, hinta FROM Tuotteet ORDER BY nimi DESC;'

        const splitSelectAllOrderByCommand = cleanCommand(
            selectAllOrderByCommand
        )

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.updateState(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectColumnsFromTable() with command.where and command.orderBy', () => {
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

        const splitCommandArray = commands.map((input) => cleanCommand(input))

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))
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

        const commandArray = cleanCommand(selectCommand)

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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta>5 ORDER BY hinta ASC;'

        const commandArray = cleanCommand(selectCommand)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)

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

        const commandArray = cleanCommand(selectCommand)
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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta<5 ORDER BY hinta DESC;'

        const commandArray = cleanCommand(selectCommand)
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
            'SELECT id, nimi, hinta FROM Tuotteet WHERE hinta<=3 ORDER BY hinta DESC;'

        const commandArray = cleanCommand(selectCommand)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })
})
