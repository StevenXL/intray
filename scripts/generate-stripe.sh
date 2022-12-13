sudo rm -rf intray-stripe-client
nix build .#generatedIntrayStripeCode
cp -rHL result intray-stripe-client
sudo chmod -R +rwx intray-stripe-client
