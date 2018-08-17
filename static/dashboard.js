function updateDeploymentCard() {
    var selectbox = document.querySelector('#deployment-selector');
    var card = document.querySelector('#deploy');
    var header = document.querySelector('#deploy .card-header');
    var warn = card.dataset.warnDeployments.split(/\s+/);
    var isProduction = warn.includes(selectbox.value);

    card.classList.toggle('border-danger', isProduction);
    header.classList.toggle('bg-danger', isProduction);
    header.classList.toggle('text-white', isProduction);
}

document.querySelector('#deployment-selector').addEventListener('change', updateDeploymentCard);

if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", updateDeploymentCard);
} else {  // `DOMContentLoaded` already fired
    updateDeploymentCard();
}
