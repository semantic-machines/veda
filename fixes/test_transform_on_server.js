//v-te:inputIndividual d:e0hjc2v804u7itfy8oc6h51x
//v-te:testedTransform mnd-s:meetingRouteStartFormToNet

var input = get_individual(ticket, "d:nkxoffaldcqflkxnu2d5ggwu");
print("input", JSON.stringify(input));

var transform = get_individual(ticket, "mnd-s:meetingRouteStartFormToNet");
print("trans", JSON.stringify(transform));

var output = transformation(ticket, input, transform, null, null);
print("output", JSON.stringify(output));
