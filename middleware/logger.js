const requestLogger = (request, response, next) => {
    console.log('Body:  ', request.body)
    next()
}

module.exports = requestLogger
