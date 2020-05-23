const Joi = require('@hapi/joi')

const SelectAllSchema = Joi.object({
    name: Joi.string().required().valid('SELECT *').insensitive().messages({
        'any.only': 'Query must begin with SELECT *',
        'any.required': 'Query must begin with SELECT *',
    }),

    from: Joi.string().required().valid('FROM').insensitive().messages({
        'any.only': 'SELECT * must be followed by FROM',
        'any.required': 'SELECT * must be followed by FROM',
    }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = SelectAllSchema
