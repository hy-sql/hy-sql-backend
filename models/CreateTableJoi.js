const Joi = require('@hapi/joi')

const CreateTable = Joi.object({
    command: Joi.string()
        .alphanum()
        .min(3)
        .max(30)
        .valid('CREATE TABLE')
        .required()
        .messages({
            'any.only': 'Command must be "CREATE TABLE"',
        }),

    tableName: Joi.string().alphanum().min(2).max(64).required().messages({
        'string.base': 'this is not a string',
    }),

    openingBracket: Joi.string().valid('(').required(),

    closingBracket: Joi.string().valid(')').required(),

    finalSemicolon: Joi.string().valid(';').required(),
})

const object = {
    command: 'CREATE TABLEA',
    tableName: '1',
    openingBracket: '(',
    columns: [
        { name: 'id', type: 'INTEGER', primaryKey: true },
        { name: 'nimi', type: 'TEXT', primaryKey: false },
        { name: 'hinta', type: 'INTEGER', primaryKey: false },
    ],
    closingBracket: ')',
    finalSemicolon: 'null',
}

const result = CreateTable.validate(object, { abortEarly: false })

console.log(result.error)
