/* eslint-disable no-unused-vars */
const { UpdateSchema } = require('../models/UpdateSchema')

const parseCommand = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList[0],
        tableName: fullCommandAsStringList[1],
        set: fullCommandAsStringList[2],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
                ? ';'
                : undefined,
    }

    return UpdateSchema.validate(parsedCommand)
}

module.exports = { parseCommand }
