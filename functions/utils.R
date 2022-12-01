
# colorspace
colorspace_seq1 <- c('#7ABD7E')
colorspace_seq2 <- c('#118ab2')


# colorspace_seq1 <- sequential_hcl(8, palette = "Sunset")
   
fun_double_gauge <- function(gauge_val= num_val, gauge_name= 'text_var') {

gauge_val <- as.numeric(as.character(gauge_val))   

e_charts() |>
         e_gauge(gauge_val, 
               gauge_name,
                min=0, 
                max=100,
                splitNumber = 10,
                center = list('50%', '60%')
                , startAngle = 200
                , endAngle = -20 
                
                ,itemStyle=list(
                    #color= '#FFAB91'
                    color = '#56cfe1'
                ),
                progress=list(
                    show= TRUE,
                    width= 30
                ),
                pointer=list(
                    show= FALSE
                )

                , axisLine=list(
                    lineStyle=list(
                    width= 30
                    )
                ),
                axisTick=list(
                    distance= -45,
                    splitNumber= 5,
                    lineStyle=list(
                    width= 2,
                    color= '#999'
                    )
                )   

                , splitLine=list(
                        distance= -52,
                        length= 14,
                        lineStyle=list(
                        width= 3,
                        color= '#999'
                        )
                ),
                    axisLabel=list(
                        distance= -20,
                        color= '#999',
                        fontSize= 20
                    ),
                    anchor=list(
                        show= FALSE
                    )

               , title=list(
                    show= FALSE
                ),
                detail=list(
                    valueAnimation=TRUE,
                    width= '60%',
                    lineHeight= 40,
                    borderRadius= 8,
                    offsetCenter= list(0, '-15%'),
                    fontSize= 60,
                    fontWeight= 'bolder',
                    color= 'auto'
                )

                ) |>

                e_gauge(gauge_val,
                gauge_name,
                min=0, 
                max=100,
                splitNumber = 10,
                center = list('50%', '60%')
                , startAngle = 200
                , endAngle = -20 

                , itemStyle=list(
                   # color= '#FD7347'
                    color = '#5390d9'
                ),
                progress=list(
                    show= TRUE,
                    width= 8
                ),
                pointer=list(
                    show= FALSE
                ),
                axisLine=list(
                    show= FALSE
                ),
                axisTick=list(
                    show= FALSE
                ),
                splitLine=list(
                    show= FALSE
                ),
                axisLabel=list(
                    show= FALSE
                ),
                detail=list(
                    show= FALSE
                )

                )

}

###

fun_pp1 <- function(gauge_val = 82, gauge_name = 'PACE', r1='40%', c1='15%', c2='40%'){
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
                    length= '20%',
                    width = 20,
                    offsetCenter=list(0, '-68%'),
                    itemStyle=list(color= 'auto')
                    ),

                    detail=list(
                    fontSize= 22,
                    offsetCenter=list(0, '-30%'),
                    valueAnimation= TRUE,
                    formatter= '{value}',
                    color= 'auto'
                    )

                    , title= list(
                    offsetCenter=list(0, '5%'),
                    fontSize= 16
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





##
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

