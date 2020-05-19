const Joi = require('@hapi/joi')

const CreateTable = Joi.object({
    name: Joi.string()
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

    columns: Joi.array().min(1).required(),

    closingBracket: Joi.string().valid(')').required(),

    finalSemicolon: Joi.string().valid(';').required(),
})

module.exports = CreateTable
