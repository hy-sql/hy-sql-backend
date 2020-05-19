let yup = require('yup')

let schema = yup.object().shape({
    name: yup
        .string()
        .required()
        .matches(/^CREATE TABLE$/),
    tableName: yup.string().required().max(64),
    openingBracket: yup.string().required().matches(/\(/),
    columns: yup.array().required().min(2).max(64),
    closingBracket: yup.string().required().matches(/\)/),
    finalSemicolon: yup.string().required().matches(';'),
})

const object = {
    name: 'CREATE TABLEA',
    tableName: 'Tuotteet',
    openingBracket: '(',
    columns: [
        { name: 'id', type: 'INTEGER', primaryKey: true },
        { name: 'nimi', type: 'TEXT', primaryKey: false },
        { name: 'hinta', type: 'INTEGER', primaryKey: false },
    ],
    closingBracket: ')',
    finalSemicolon: ';',
}

const validate = async (object) => {
    try {
        const result = await schema.validate(object)
        console.log(result)
    } catch (error) {
        console.log('catched')
        console.log(error)
    }
}

validate(object)
