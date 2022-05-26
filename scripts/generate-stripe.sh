sudo rm -rf intray-stripe-client
cp -rHL $(nix-build nix/pkgs.nix -A generatedIntrayStripeCode) intray-stripe-client
sudo chmod -R +rwx intray-stripe-client
