const Joi = require('@hapi/joi')

const UpdateSchema = Joi.object({
    name: Joi.string().required().valid('UPDATE').insensitive().messages({}),

    tableName: Joi.string()
        .required()
        .pattern(/^\w+$/)
        .min(1)
        .max(64)
        .messages({
            'string.base': 'this is not a string',
            'any.required': 'Query must contain a table name',
            'string.pattern.base':
                'Table name is invalid. Only a-z,A-Z,0-9 and _ allowed.',
            'string.max': 'The table name is too long',
        }),

    /*SET*/
    set: Joi.string().required().valid('SET').insensitive().messages({}),
    //column-value parit tähän

    // columns: Joi.array().min(1).items(ColumnsSchema).required().messages({}),

    //WHERE-parsiminen tähän TODO

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

// const ColumnsSchema = Joi.object({
//     columnName: Joi.string(),
//     sign: Joi.string(),
//     value: Joi.string(),
// })

module.exports = { UpdateSchema }
