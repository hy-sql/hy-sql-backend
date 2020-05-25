const State = require('../models/State')

const executer = (request, response, next) => {
    const state = new State([])

    if (!request.body.commandArray) {
        return response.status(400).json({
            error: 'commandArray missing',
        })
    }

    const commandArray = request.body.commandArray

    const resultArray = []

    let noErrors = true

    for (let command of commandArray) {
        if (!noErrors) {
            break
        }

        if (command.error) {
            resultArray.push(
                `${command.value.name} -query execution failed: ${command.error.details[0].message}`
            )
            noErrors = false
        } else {
            if (!command) {
                resultArray.push(
                    'Query was not recognised as any existing valid query'
                )
                noErrors = false
            } else {
                // Optimaalisesti olisi siirtää validaatiovirheet omaan virhekäsittelijään
                if (command.error) {
                    // koko error olion sijaan vain sen sisältämä viesti
                    resultArray.push(
                        `${command.value.name} -query execution failed: ${command.error.details[0].message}`
                    )
                    noErrors = false
                } else {
                    /* state voisi palauttaa udateState metodista esim. create table ja insert intolla
                'x - query was executed successfully'
                tai tarkemman onnistumisviestin kuten 'Table x created successfully'
                ja selectillä palauttaakin jo tulostaulut, jolloin tämä palautus tallennettaisiin resultArrayhin.
                Olisi informatiivisemmat onnistumisviestit/tulostaulut.
                */
                    state.updateState(command.value)
                    resultArray.push(
                        `${command.value.name} -query was executed successfully`
                    )
                }
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
