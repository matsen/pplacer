#include <Bpp/App/ApplicationTools.h>
#include <Bpp/App/BppApplication.h>
#include <Bpp/Phyl/App/PhylogeneticsApplicationTools.h>
#include <Bpp/Seq/App/SequenceApplicationTools.h>

#include <Bpp/Numeric/Prob/DiscreteDistribution.h>
#include <Bpp/Phyl/Likelihood/RHomogeneousTreeLikelihood.h>
#include <Bpp/Seq/Container/SequenceContainer.h>
#include <Bpp/Seq/Container/SiteContainer.h>
#include <Bpp/Seq/Container/SiteContainerTools.h>
#include <Bpp/Seq/Container/VectorSiteContainer.h>
#include <Bpp/Seq/Io/ISequence.h>
#include <Bpp/Seq/Io/IoSequenceFactory.h>
#include <Bpp/Seq/SiteTools.h>

#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>

using namespace std;
using bpp::Node;
using bpp::TreeTemplate;

int main(int argc, char** argv)
{
    bpp::BppApplication lcfit_tripod_testdata(argc, argv, "lcfit_tripod_testdata");
    map<string, string> params = lcfit_tripod_testdata.getParams();
    params["input.sequence.file"] = "test.fasta";
    params["input.sequence.format"] = "Fasta";
    params["model"] = "JC69";
    params["alphabet"] = "DNA";
    params["rate_distribution"] = "Uniform";

    // Alphabet
    unique_ptr<bpp::Alphabet> alphabet(bpp::SequenceApplicationTools::getAlphabet(params, "", false));
    // Sites
    unique_ptr<bpp::VectorSiteContainer> sites(bpp::SequenceApplicationTools::getSiteContainer(alphabet.get(), params));
    // Model, rates
    unique_ptr<bpp::SubstitutionModel> model(bpp::PhylogeneticsApplicationTools::getSubstitutionModel(alphabet.get(), sites.get(), params));
    bpp::SiteContainerTools::changeGapsToUnknownCharacters(*sites);
    unique_ptr<bpp::DiscreteDistribution> rate_dist(bpp::PhylogeneticsApplicationTools::getRateDistribution(params));

    if(sites->getNumberOfSequences() != 3)
        throw std::runtime_error("Tripod requires three sequences, not " + std::to_string(sites->getNumberOfSequences()));


    // Set up the tripod
    Node *x        = new Node(0, "x"),
         *y        = new Node(1, "y"),
         *z        = new Node(2, "z"),
         *internal = new Node(3);
    internal->addSon(y);
    internal->addSon(x);
    internal->addSon(z);

    TreeTemplate<Node> tree(internal);

    // ML distance between y and z via Jukes-Cantor
    const double t = 0.3999739;


    cout << "c,tx,l\n";
    for(double c = 1e-6; c < t - 1e-6; c += 0.01) {
        for(double tx = 1e-6; tx <= 0.8; tx += 0.01) {
            x->setDistanceToFather(tx);
            y->setDistanceToFather(c);
            z->setDistanceToFather(t - c);

            bpp::RHomogeneousTreeLikelihood like(tree, *sites, model.get(), rate_dist.get(), false, false, false);
            like.initialize();
            like.computeTreeLikelihood();
            const double l = like.getLogLikelihood();
            cout << c << ',' << tx << ',' << setprecision(12) << l << endl;
        }
    }

    return 0;
}
