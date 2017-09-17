for(i in 3:N)
  trend[i] ~ normal(2*trend[i-1] - trend[i-2], s_trend);

//基本部分
for(i in 7:N)
  s[i] ~ normal(-s[i-1]-s[i-2]-s[i-3]-s[i-4]-s[i-5]-s[i-6], s_s);


//祝日効果
for(i in 1:7)
  week[i] = s[i] + D1[i] * b1 * (s[i - Wday[i]] - s[i]);
//祝日前日効果
for(i in 8:N)
  week[i] = s[i] + D1[i]*b1*(s[i-Wday[i]]-s[i])
            +D2[i] * (b2*(s[i - Wday[i]-2]-s[i]) + b3*(s[i - Wday[i]-1]-s[i]));

//雨の効果
for(i in 1:N)
  rain[i] = c_rain * Rain_val[i]

//イベントの効果
for (i in 2:N)
  c_event[i] ~ normal(c_event[i-1], s_event);
for(i in 1:N)
  event[i] = c_event[i] * Event_val[i];

//ar項(2次のarモデルを想定)
for(i in 3:N)
  ar[i] ~ normal(c_ar[1*ar[i-1] + c_ar[2]*ar[i-2], s_ar]);

