# SynchronizedScalaCollections

As Scala mutable synchronized collections are deprecated and it the recommended replacements are Java's concurrent collections that have a different API and Java return types,
I thought it would be a good idea to provide Scala wrappers around these Java classes to provide an API with the most common Scala Collection API methods.

There are only two collections ready yet and some further reviewing would be needed to make sure these calls are synchronous in all situations so feel free to contribute.
