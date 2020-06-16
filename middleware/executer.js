const State = require('../models/State')
const StateService = require('../services/StateService')

/**
 * Handles executing commands. Expects the parsed commands to be found in request.parsedCommands
 * in an array. Each query is passed to StateService for handling and from the result either an error
 * object or the result is added to the maintained array of results. Execution of commands is halted
 * when an error or no command is found. When execution is ready the result array and the current tables
 * in State are placed into request.resultArray as an object: { resultArray, state: stateArray }
 */
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

    request.resultArray = {
        resultArray,
        state: stateArray,
    }

    next()
}

module.exports = executer
