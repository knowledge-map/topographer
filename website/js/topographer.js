Parse.initialize("HCqG8ipL0wyGCrMl60mKk1w9EqWTfmUl2k05pnJm", "GX0soAeJUyTHePsm83pcE95kydFz7y55mplHSd1A");

var Resource = Parse.Object.extend("Resource");
var Concept = Parse.Object.extend("Concept");
var ConceptMap = Parse.Object.extend("ConceptMap");
var ConceptRelationship = Parse.Object.extend("ConceptRelationship");

function saveResource(form) {
    var resource = new Resource();

    resource.save({
        name: form.name.value,
        URL: form.URL.value,
    }, {
        success: function() {console.log("success", arguments);},
        error: function() {console.log("failure", arguments);}
    });

    var relationships = ["teaches", "requires"];
    relationships.forEach(function(relationship) {
        var conceptTags = $(form[relationship]).tagit("assignedTags");
        saveMappings(resource, relationship, conceptTags);
    });

    return false;
}

function saveMappings(resource, relationshipName, conceptNames) {
    var concepts = getConcepts(conceptNames);
    var relationship = getRelationship(relationshipName);
    var maps = getConceptMaps(resource);

    Parse.Promise.when([concepts, relationship, maps])
    .then(function(concepts, relationship, maps) {
        console.log(maps);
        maps.forEach(function(map) {
            map.destroy();
        });

        conceptNames.forEach(function(conceptName) {
            var existingConcepts = concepts.filter(function(concept) {
                return concept.get("name") == conceptName;
            });

            if (existingConcepts.length > 0) {
                existingConcepts.forEach(function(concept) {
                    saveMapping(resource, relationship, concept);
                });
            } else {
                var concept = new Concept();
                concept.save({name: conceptName})
                .then(function(concept) {
                    saveMapping(resource, relationship, concept);
                });
            }
        });
    });
}

function saveMapping(resource, relationship, concept) {
    var conceptMap = new ConceptMap();

    conceptMap.save({
        resource: resource,
        relationship: relationship,
        concept: concept,
    });
}

function getResource(resourceId) {
    var query = new Parse.Query(Resource);

    return query.get(resourceId);
}

function getConcepts(conceptNames) {
    var query = new Parse.Query(Concept);
    query.containedIn("name", conceptNames);

    return query.find();
}

function getConceptMaps(resource) {
    var query = new Parse.Query(ConceptMap);
    if (resource.isNew()) {
        return Parse.Promise.as([]);
    } else {
        query.equalTo("resource", resource);
        return query.find();
    }
}

function getRelationship(relationshipName) {
    var query = new Parse.Query(ConceptRelationship);
    query.equalTo("name", relationshipName)

    return query.find()
        .then(function(relationships) {
            return relationships.length > 0 ? relationships[0] : null;
        });
}
