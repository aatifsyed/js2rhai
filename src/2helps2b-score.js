var calc_output = [];
var birds = parseFloat(birds);
var lpds = parseFloat(lpds);
var prior = parseFloat(prior);
var spor = parseFloat(spor);
var freq = parseFloat(freq);
var plus = parseFloat(plus);
var score = birds + lpds + prior + spor + freq + plus;
var risk;
if (score == 0) {
    risk = 5;
} else if (score == 1) {
    risk = 12;
} else if (score == 2) {
    risk = 27;
} else if (score == 3) {
    risk = 50;
} else if (score == 4) {
    risk = 73;
} else if (score == 5) {
    risk = 88;
} else {
    risk = ">95";
}
var rec;
if (score == 0) {
    rec = 0;
} else if (score == 1) {
    rec = 12;
} else {
    rec = 24;
}
calc_output.push({
    name: "mini",
    value: score,
    value_text: "points",
    message: "2HELPS2B Score",
});
calc_output.push({
    name: "Score",
    value: score,
    value_text: "points",
    message: "2HELPS2B Score",
});
calc_output.push({
    name: "Risk",
    value: risk,
    value_text: "%",
    message: "Risk of seizure",
});
calc_output.push({
    name: "Monitoring",
    value: rec,
    value_text: "hours",
    message: "Recommended length of additional cEEG monitoring",
});
