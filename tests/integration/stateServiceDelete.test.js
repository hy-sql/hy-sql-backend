const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')
const SQLError = require('../../models/SQLError')

const initialiseState = () => {
    const initTable = new Map([
        [
            'Tuotteet',
            {
                name: 'Tuotteet',
                columns: [
                    {
                        name: 'id',
                        type: 'INTEGER',
                        constraints: ['PRIMARY KEY'],
                    },
                    {
                        name: 'nimi',
                        type: 'TEXT',
                        constraints: [],
                    },
                    {
                        name: 'hinta',
                        type: 'INTEGER',
                        constraints: [],
                    },
                ],
                rows: [
                    {
                        id: 1,
                        nimi: 'omena',
                        hinta: 7,
                    },
                    {
                        id: 2,
                        nimi: 'appelsiini',
                        hinta: 5,
                    },
                    {
                        id: 3,
                        nimi: 'banaani',
                        hinta: 1,
                    },
                    {
                        id: 4,
                        nimi: 'maito',
                        hinta: 4,
                    },
                    {
                        id: 4,
                        nimi: 'porkkana',
                        hinta: 4,
                    },
                ],
            },
        ],
    ])

    const state = new State(initTable)
    const stateService = new StateService(state)

    return {
        state,
        stateService,
    }
}

const parseCommand = (command) => {
    const fullCommandAsStringArray = splitCommandIntoArray(command)
    return commandService.parseCommand(fullCommandAsStringArray)
}

describe('Valid DELETE-command', () => {
    const queries = [
        'DELETE FROM Tuotteet;',
        "DELETE FROM Tuotteet WHERE nimi='maito';",
        'DELETE FROM Tuotteet WHERE hinta=1;',
        'DELETE FROM Tuotteet WHERE hinta<=4;',
        'DELETE FROM Tuotteet WHERE hinta>=4;',
        'DELETE FROM Tuotteet WHERE hinta<4;',
        'DELETE FROM Tuotteet WHERE hinta>4;',
        'DELETE FROM Tuotteet WHERE hinta<5 OR hinta>5;',
        "DELETE FROM Tuotteet WHERE hinta<>4 OR nimi<>'maito';",
        'DELETE FROM Tuotteet WHERE hinta<4 AND hinta>4;',
        'DELETE FROM Tuotteet WHERE hinta<LENGTH(nimi);',
        'DELETE FROM Tuotteet WHERE hinta<4 OR hinta>=4;',
    ]

    test.each(queries)(
        'returns result message and no error from stateService',
        (command) => {
            const { stateService } = initialiseState()
            const parsedCommand = parseCommand(command)

            const result = stateService.updateState(parsedCommand)

            expect(result.result).toBeDefined()
        }
    )

    test(`${queries[0]} deletes all rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[0])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(0)
    })

    test(`${queries[1]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[1])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.nimi === 'maito')).toHaveLength(0)
    })

    test(`${queries[2]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[2])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.hinta === 1)).toHaveLength(0)
    })

    test(`${queries[3]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[3])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(2)
        expect(rows.filter((r) => r.hinta < 5)).toHaveLength(0)
    })

    test(`${queries[4]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[4])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(1)
        expect(rows.filter((r) => r.hinta > 3)).toHaveLength(0)
    })

    test(`${queries[5]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[5])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.hinta < 4)).toHaveLength(0)
    })

    test(`${queries[6]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[6])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(3)
        expect(rows.filter((r) => r.hinta > 4)).toHaveLength(0)
    })

    test(`${queries[7]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[7])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(1)
        expect(rows).toContainEqual({ id: 2, nimi: 'appelsiini', hinta: 5 })
    })

    test(`${queries[8]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[8])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toHaveLength(1)
        expect(rows).toContainEqual({ id: 4, nimi: 'maito', hinta: 4 })
    })

    test(`${queries[9]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[9])

        const rowsBefore = state.getTableByName('Tuotteet').rows
        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toEqual(rowsBefore)
    })

    test(`${queries[10]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[10])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toContainEqual({ id: 1, nimi: 'omena', hinta: 7 })
    })

    test(`${queries[11]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[11])

        stateService.updateState(parsedCommand)
        const rows = state.getTableByName('Tuotteet').rows

        expect(rows).toEqual([])
    })
})

describe('If referenced table does not exist ', () => {
    test('returns result message and no error from stateService', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const parsedCommand = parseCommand('DELETE FROM Tuotteet;')

        expect(() => stateService.updateState(parsedCommand)).toThrowError(
            new SQLError('No such table Tuotteet')
        )
    })
})
