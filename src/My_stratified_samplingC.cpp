#include <Rcpp.h>
#include <random>
using namespace Rcpp;
//' @name My_stratified_samplingC
//' @title A Stratified sampling function using Rcpp
//' @description Samples are drawn according to the idea of stratified sampling using Rcpp
//' @useDynLib SA24168406
//' @param data The data that contains the id, category, and value columns
//' @param samplesPerCategory The number of samples for each category
//' @return A list of the sampling results including id and category \code{n}
//' @examples
//' \dontrun{
//'     data <- data.frame(id = 1:9, category = rep(c("A", "B", "C"), each = 3), value = rnorm(9))
//'     samples_per_category <- list(A = 1, B = 2, C = 1)
//'     sampled_data <- My_stratified_samplingC(data, samples_per_category)
//' }
//' @export

// 定义数据点结构体
struct DataPoint {
  int id;
  String category;
};

// 分层抽样函数
// data: 数据框，包含id、category和value列
// samplesPerCategory: 每个类别的样本数量
//[[Rcpp::export]]
List My_stratified_samplingC(DataFrame data, const List& samplesPerCategory) {
  std::default_random_engine generator;
  std::vector<DataPoint> dataPoints;
  std::vector<DataPoint> sample;

  // 将R数据框转换为C++数据点向量
  IntegerVector id_1 = data["id"];
  CharacterVector category_1 = data["category"];
  for (int i = 0; i < data.nrows(); ++i) {
    dataPoints.push_back({id_1[i], category_1[i]});
  }

  // 遍历每个类别和对应的样本数量
  CharacterVector names = samplesPerCategory.names();
  for (int i = 0; i < samplesPerCategory.length(); ++i) {
    int numSamples = samplesPerCategory[i];

    // 找出当前类别的所有数据点
    std::vector<DataPoint> categoryData;
    for (const auto& point : dataPoints) {
      if (point.category == names[i]) {
        categoryData.push_back(point);
      }
    }

    // 检查是否有足够的数据点
    if (categoryData.size() < static_cast<size_t>(numSamples)) {
      throw std::runtime_error("Not enough data points in category: " + names[i]);
    }

    // 打乱当前类别的数据点
    std::shuffle(categoryData.begin(), categoryData.end(), generator);

    // 添加所需数量的样本到结果中
    sample.insert(sample.end(), categoryData.begin(), categoryData.begin() + numSamples);
  }

  // 将样本转换回R的数据框
  IntegerVector id = IntegerVector(sample.size());
  CharacterVector category = CharacterVector(sample.size());
  for (size_t i = 0; i < sample.size(); ++i) {
    id[i] = sample[i].id;
    category[i] = sample[i].category;
  }
  DataFrame sampleDF = DataFrame::create(
    Named("id") = id,
    Named("category") = category
  );

  return List::create(sampleDF);
}
