#ifndef STANR_RUN_DIAGNOSE_HPP
#define STANR_RUN_DIAGNOSE_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stan/callbacks/writer.hpp>
#include <stan/model/finite_diff_grad.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/services/util/create_rng.hpp>
#include <stan/services/util/initialize.hpp>
#include <cmath>
#include <iomanip>
#include <sstream>
#include "r_interrupt.hpp"
#include "r_logger.hpp"
#include <stanr/r_data_context.hpp>

namespace stanr {
  // Inlines stan::services::diagnose::diagnose() so the gradient check
  // results come back as full-precision vectors instead of the 6-sig-fig
  // formatted text stan::model::test_gradients() writes. The logged table
  // matches test_gradients() line for line.
  template <class Model>
  cpp11::writable::list run_diagnose(Model& model, cpp11::list args) {
    const unsigned int seed = stanr::as_cpp<unsigned int>(args["seed"]);
    const unsigned int chain_id = stanr::as_cpp<unsigned int>(args["id"]);
    const double init_radius = stanr::as_cpp<double>(args["init_radius"]);
    const double epsilon = stanr::as_cpp<double>(args["epsilon"]);
    const double error_thresh = stanr::as_cpp<double>(args["error"]);
    const bool verbose = stanr::as_cpp<bool>(args["verbose"]);
    const bool show_exceptions = stanr::as_cpp<bool>(args["show_exceptions"]);

    cpp11::list init_list = args["init"];

    stanr::r_data_context init_ctx(init_list, args["init_declarations"]);
    stan::callbacks::writer init_writer;
    stanr::r_logger logger(verbose, show_exceptions);
    stanr::r_interrupt interrupt(true);

    stan::rng_t rng = stan::services::util::create_rng(seed, chain_id);
    std::vector<int> params_i;
    std::vector<double> params_r = stan::services::util::initialize(
        model, init_ctx, rng, init_radius, false, logger, init_writer);

    logger.info("TEST GRADIENT MODE");

    std::stringstream msg;
    std::vector<double> grad;
    const double lp = stan::model::log_prob_grad<true, true>(
        model, params_r, params_i, grad, &msg);
    if (msg.str().length() > 0) logger.info(msg);

    std::stringstream fd_msg;
    std::vector<double> grad_fd;
    stan::model::finite_diff_grad<false, true, Model>(
        model, interrupt, params_r, params_i, grad_fd, epsilon, &fd_msg);
    if (fd_msg.str().length() > 0) logger.info(fd_msg);

    std::stringstream lp_msg;
    lp_msg << " Log probability=" << lp;
    logger.info("");
    logger.info(lp_msg);
    logger.info("");

    std::stringstream header;
    header << std::setw(10) << "param idx" << std::setw(16) << "value"
           << std::setw(16) << "model" << std::setw(16) << "finite diff"
           << std::setw(16) << "error";
    logger.info(header);

    int num_failed = 0;
    std::vector<double> grad_error(params_r.size());
    for (size_t k = 0; k < params_r.size(); ++k) {
      grad_error[k] = grad[k] - grad_fd[k];
      std::stringstream line;
      line << std::setw(10) << k << std::setw(16) << params_r[k]
           << std::setw(16) << grad[k] << std::setw(16) << grad_fd[k]
           << std::setw(16) << grad_error[k];
      logger.info(line);
      if (std::fabs(grad_error[k]) > error_thresh) num_failed++;
    }

    logger.flush();
    cpp11::writable::strings output = cpp11::as_sexp(logger.history());

    return cpp11::writable::list({
      cpp11::named_arg("num_failed") = num_failed,
      cpp11::named_arg("return_code") = num_failed == 0 ? 0 : 1,
      cpp11::named_arg("output") = output,
      cpp11::named_arg("lp") = lp,
      cpp11::named_arg("value") = params_r,
      cpp11::named_arg("model") = grad,
      cpp11::named_arg("finite_diff") = grad_fd,
      cpp11::named_arg("error") = grad_error
    });
  }
}

#endif
