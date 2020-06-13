const State = require('../models/State')
const StateService = require('../services/StateService')

const executer = (request, response, next) => {
    const state = new State(new Map())
    const stateService = new StateService(state)

    const parsedCommands = request.parsedCommands

    const resultArray = []

    let errors = false

    for (let command of parsedCommands) {
        if (errors) break

        if (!command) {
            resultArray.push(
                'Query was not recognised as any existing valid query'
            )
            errors = true
        } else if (command.error) {
            resultArray.push({
                error: `${command.value.name} -query execution failed: ${command.error.details[0].message}`,
            })
            errors = true
        } else {
            const result = stateService.updateState(command.value)
            if (result.error) errors = true
            resultArray.push(result)
        }
    }

    const stateArray = Array.from(state.tables.values())

    console.log('resultArray:', resultArray)
    console.log('stateArray:', stateArray)

    request.resultArray = {
        resultArray,
        state: stateArray,
    }

    next()
}

module.exports = executer
