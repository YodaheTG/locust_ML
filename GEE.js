var years =ee.List.sequence(2013,2017)
var months = ee.List.sequence(1,12) 
var nas = ee.ImageCollection(NASA);
var sm_data =ee.ImageCollection(NASA
 .filterDate('2013-01-01', '2017-01-01')
 .select ("ssm"));

var test = ee.Image (NASA
    .filterDate('2013-01-01', '2013-01-30')
    .select ("ssm"));
var median = sm_data.mean();
//print (sm_data)    
//print (median)

var mean_value = ee.ImageCollection.fromImages(
  years.map (function(y){
  var w =  nas.filter(ee.Filter.calendarRange(y,y,'year'));
  return w;
  })
  );
//print (mean_value)
    
var test2 = nas.filter(ee.Filter.calendarRange(2010,2010,'year'));
//print (mean_value)
var addTime = function(image) {
 // return image.filter(ee.Filter.calendarRange(2010,2010,'year'));
 var selected = nas.filter(ee.Filter.calendarRange(2013,2017,'year'));
 return months.map 
 return selectedb;
};
print(nas.map(addTime));   
