const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    columnName: Joi.string(),
    value: Joi.any(), //??
})

const UpdateSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .required()
        .valid('UPDATE')
        .insensitive()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'any.only': 'Query must begin with UPDATE',
            'any.required': 'Query must begin with UPDATE',
        }),

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
    set: Joi.string().required().valid('SET').insensitive().messages({
        'any.required': 'Query must contain SET',
    }),
    //column-value parit tähän

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({}),

    //WHERE-parsiminen tähän TODO

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { UpdateSchema }
