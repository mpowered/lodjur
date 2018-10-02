function setupWarnings() {

    var card = document.querySelector('#deploy');
    var header = document.querySelector('#deploy .card-header');

    function updateDeploymentCard(e) {
        var selectBox = e.target;
        var selectedOption = selectBox.options[selectBox.selectedIndex];
        var warn = !!selectedOption.dataset.warn;

        card.classList.toggle('border-danger', warn);
        header.classList.toggle('bg-danger', warn);
        header.classList.toggle('text-white', warn);
    }

    document.querySelector('#deployment-selector').addEventListener('change', updateDeploymentCard);
    updateDeploymentCard();
};

if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", setupWarnings);
} else {  // `DOMContentLoaded` already fired
    setupWarnings();
}
