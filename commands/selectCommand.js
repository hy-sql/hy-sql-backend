const { cleanStringArray } = require('./parserStringCleaners')

const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    console.log(fullCommandAsStringList)

    //const columns = []
    let laskuri = 1
    while (laskuri < fullCommandAsStringList.length) {
        if (fullCommandAsStringList[laskuri].toUpperCase() === 'FROM') break
        laskuri++
    }
    if (laskuri === fullCommandAsStringList.length) {
        console.log('laskimena käytetään')
        return 'hupsista'
    }
    console.log(cleanStringArray(fullCommandAsStringList.slice(1, laskuri)))

    const parsedCommand = {
        name: fullCommandAsStringList[0],
        fromKeyword: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    return selectSchema.validate(parsedCommand)
}

module.exports = { parseCommand }
