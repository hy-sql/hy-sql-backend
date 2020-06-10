const State = require('../models/State')
const StateService = require('../services/StateService')

const executer = (request, response, next) => {
    const state = new State(new Map())
    const stateService = new StateService(state)

    const parsedCommands = request.parsedCommands

    const resultArray = []

    let noErrors = true

    for (let command of parsedCommands) {
        if (!noErrors) {
            break
        }

        if (!command) {
            resultArray.push(
                'Query was not recognised as any existing valid query'
            )
            noErrors = false
        } else {
            if (command.error) {
                resultArray.push({
                    error: `${command.value.name} -query execution failed: ${command.error.details[0].message}`,
                })
                noErrors = false
            } else {
                /* State palauttaa updateState:sta tuloksen muodossa { result: result }
                tai vastaavasti virheilmoituksen muodossa { error: error }. Lisäksi SELECT *
                palauttaa taulun rivit muodossa { result: result, rows: [] }.
                Jos CREATE TABLE -lauseessa yritetään muodostaa duplikaattisarakkeita palautetaan lista
                virheviestejä muodossa { error: [] }
                */
                resultArray.push(stateService.updateState(command.value))
            }
        }
    }

    console.log('resultArray:', resultArray)
    console.log('stateArray:', state)

    request.resultArray = {
        resultArray,
        state,
    }

    next()
}

module.exports = executer
