const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')
const cleanCommand = require('../utils/cleanCommand')

const initialiseState = () => {
    const initArray = [
        {
            name: 'Products',
            columns: [
                {
                    name: 'id',
                    type: 'INTEGER',
                    constraints: ['PRIMARY KEY'],
                },
                {
                    name: 'name',
                    type: 'TEXT',
                    constraints: [],
                },
                {
                    name: 'price',
                    type: 'INTEGER',
                    constraints: [],
                },
            ],
            rows: [
                {
                    id: 1,
                    name: 'apple',
                    price: 7,
                },
                {
                    id: 2,
                    name: 'orange',
                    price: 5,
                },
                {
                    id: 3,
                    name: 'banana',
                    price: 1,
                },
                {
                    id: 4,
                    name: 'milk',
                    price: 4,
                },
                {
                    id: 4,
                    name: 'carrot',
                    price: 4,
                },
            ],
        },
    ]

    const state = new State(initArray)
    const stateService = new StateService(state)

    return {
        state,
        stateService,
    }
}

const parseCommand = (command) => {
    const fullCommandAsStringList = cleanCommand(command)
    return commandService.parseCommand(fullCommandAsStringList)
}

describe('Valid DELETE-command', () => {
    const queries = [
        'DELETE FROM Products;',
        "DELETE FROM Products WHERE name='milk';",
        'DELETE FROM Products WHERE price=1;',
        'DELETE FROM Products WHERE price<=4;',
        'DELETE FROM Products WHERE price>=4;',
        'DELETE FROM Products WHERE price<4;',
        'DELETE FROM Products WHERE price>4;',
    ]

    test.each(queries)(
        'returns result message and no error from stateService',
        (command) => {
            const { stateService } = initialiseState()
            const parsedCommand = parseCommand(command)
            expect(parsedCommand.error).toBeUndefined()

            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).toBeDefined()
            expect(result.error).toBeUndefined()
        }
    )

    test(`${queries[0]} deletes all rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[0])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(0)
    })

    test(`${queries[1]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[1])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.name === 'milk')).toHaveLength(0)
    })

    test(`${queries[2]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[2])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.price === 1)).toHaveLength(0)
    })

    test(`${queries[3]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[3])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(2)
        expect(rows.filter((r) => r.price < 5)).toHaveLength(0)
    })

    test(`${queries[4]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[4])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(1)
        expect(rows.filter((r) => r.price > 3)).toHaveLength(0)
    })

    test(`${queries[5]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[5])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(4)
        expect(rows.filter((r) => r.price < 4)).toHaveLength(0)
    })

    test(`${queries[6]} deletes the correct rows from the table`, () => {
        const { state, stateService } = initialiseState()
        const parsedCommand = parseCommand(queries[6])
        expect(parsedCommand.error).toBeUndefined()

        stateService.updateState(parsedCommand.value)
        const rows = state.tables[0].rows

        expect(rows).toHaveLength(3)
        expect(rows.filter((r) => r.price > 4)).toHaveLength(0)
    })
})

describe('If referenced table does not exist ', () => {
    test('returns result message and no error from stateService', () => {
        const state = new State([])
        const stateService = new StateService(state)
        const parsedCommand = parseCommand('DELETE FROM Products;')
        expect(parsedCommand.error).toBeUndefined()

        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeDefined()
        expect(result.error).toBe('No such table Products')
        expect(result.result).toBeUndefined()
    })
})
