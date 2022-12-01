
#? colorspace
colorspace_seq1 <- c('#7ABD7E')
colorspace_seq2 <- c('#118ab2')

#?
fun_pp2 <- function(gauge_val = 82, gauge_name = 'PACE', r1='40%', c1='15%', c2='40%'){
               
                e_charts() |>
                e_gauge(gauge_val, 
                gauge_name,
                min=0, 
                max=100,
                radius= r1,
                splitNumber = 4,
                center = list(c1, c2)
                , startAngle = 200
                , endAngle = -20 
                , axisLine=list(
                    lineStyle=list(
                    width= 8,
                    color=list(
                        list(0.25, '#FF6961'),
                        list(0.50, '#FFB54C'),
                        list(0.75, '#F8D66D'),
                        list(1, '#7ABD7E')
                    )
                    )
                )
             , axisTick=list(show=FALSE)
             , splitLine=list( 
                distance= -8,
                length= 6,
                lineStyle=list(
                width= 2,
                color='#999'
                    ))
             , axisLabel=list(
                distance= -12,
                color= '#999',
                fontSize= 8
             )
             , pointer = list(
                    icon= 'path://M12.8,0.7l12,40.1H0.7L12.8,0.7z',
                    length= '30%',
                    width = 8,
                    offsetCenter=list(0, '-68%'),
                    itemStyle=list(color= 'auto')
                    ),

                    detail=list(
                    fontSize= 16,
                    offsetCenter=list(0, '-30%'),
                    valueAnimation= TRUE,
                    formatter= '{value}',
                    color= 'auto'
                    )

                    , title= list(
                    offsetCenter=list(0, '5%'),
                    fontSize= 12
                    )

                    , itemStyle= list(
                    color= '#58D9F9',
                    shadowColor= 'rgba(0,138,255,0.45)',
                    shadowBlur= 4,
                    shadowOffsetX= 2,
                    shadowOffsetY= 2
                    )  

                    )
} 

