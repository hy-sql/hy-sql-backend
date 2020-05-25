const commands = require('../commands')
const CommandSchema = require('../models/CommandSchema')
const State = require('../models/State')

const executer = (request, response, next) => {
    const state = new State([])

    if (!request.body.commandArray) {
        return response.status(400).json({
            error: 'commandArray missing',
        })
    }

    const commandArray = request.body.commandArray

    console.log(commandArray)

    const result = CommandSchema.validate(commandArray)

    console.log(result)

    const resultArray = []

    let noErrors = true

    for (let input of commandArray) {
        if (!noErrors) {
            break
        }

        const singleCommandAsStringArray = input
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(?=;)/)

        const command = commands.find((c) =>
            c.isCommand(singleCommandAsStringArray)
        )

        if (!command) {
            resultArray.push(
                'Query was not recognised as any existing valid query'
            )
            noErrors = false
        } else {
            const parsedCommand = command.parseCommand(
                singleCommandAsStringArray
            )

            // Optimaalisesti olisi siirtää validaatiovirheet omaan virhekäsittelijään
            if (parsedCommand.error) {
                // koko error olion sijaan vain sen sisältämä viesti
                resultArray.push(
                    `${parsedCommand.value.name} -query execution failed: ${parsedCommand.error.details[0].message}`
                )
                noErrors = false
            } else {
                /* state voisi palauttaa udateState metodista esim. create table ja insert intolla
                'x - query was executed successfully'
                tai tarkemman onnistumisviestin kuten 'Table x created successfully'
                ja selectillä palauttaakin jo tulostaulut, jolloin tämä palautus tallennettaisiin resultArrayhin.
                Olisi informatiivisemmat onnistumisviestit/tulostaulut.
                */
                state.updateState(parsedCommand.value)
                resultArray.push(
                    `${parsedCommand.value.name} -query was executed successfully`
                )
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
