# calibrationProfile works

    Code
      as.data.frame(out1$data)
    Output
         method       score percentile outcome  estimate
      1     gam -0.70851973      0.025       0 0.2280920
      2     gam -0.31936674      0.050       1 0.3036047
      3     gam -0.28965963      0.075       0 0.3099183
      4     gam -0.26854024      0.100       0 0.3144504
      5     gam -0.21579546      0.125       0 0.3259235
      6     gam -0.12694900      0.150       1 0.3457244
      7     gam -0.04228831      0.175       0 0.3651047
      8     gam  0.01090609      0.200       0 0.3775141
      9     gam  0.09726820      0.225       0 0.3980008
      10    gam  0.16686510      0.250       0 0.4147783
      11    gam  0.20302153      0.275       0 0.4235757
      12    gam  0.21288369      0.300       1 0.4259840
      13    gam  0.22782199      0.325       1 0.4296386
      14    gam  0.25651524      0.350       1 0.4366793
      15    gam  0.29597482      0.375       1 0.4464032
      16    gam  0.34292989      0.400       1 0.4580272
      17    gam  0.34696534      0.425       0 0.4590285
      18    gam  0.37097378      0.450       0 0.4649923
      19    gam  0.42893547      0.475       0 0.4794282
      20    gam  0.53022892      0.500       1 0.5047243
      21    gam  0.54408209      0.525       1 0.5081849
      22    gam  0.59036428      0.550       0 0.5197388
      23    gam  0.64951309      0.575       1 0.5344723
      24    gam  0.71786211      0.600       0 0.5514213
      25    gam  0.71962806      0.625       1 0.5518578
      26    gam  0.76480480      0.650       1 0.5629956
      27    gam  0.78613501      0.675       1 0.5682330
      28    gam  0.84226992      0.700       1 0.5819407
      29    gam  0.84606645      0.725       0 0.5828634
      30    gam  0.84852363      0.750       0 0.5834604
      31    gam  0.90794320      0.775       1 0.5978169
      32    gam  0.92231184      0.800       1 0.6012645
      33    gam  1.02084053      0.825       0 0.6246197
      34    gam  1.02111683      0.850       1 0.6246845
      35    gam  1.04822764      0.875       0 0.6310149
      36    gam  1.12511740      0.900       0 0.6487194
      37    gam  1.27747068      0.925       1 0.6825841
      38    gam  1.32986226      0.950       0 0.6938183
      39    gam  1.35878197      0.975       1 0.6999234
      40    gam  1.55167777      1.000       1 0.7387937

---

    Code
      out1$citl
    Output
        outcome     score percentile                   method
      1     0.5 0.5098193      0.475 Calibration In The Large

---

    Code
      as.data.frame(out3$data)
    Output
         method       score percentile outcome  estimate
      1     gam -1.55167777      0.025       1 0.7387937
      2     gam -1.35878197      0.050       1 0.6999234
      3     gam -1.32986226      0.075       0 0.6938183
      4     gam -1.27747068      0.100       1 0.6825841
      5     gam -1.12511740      0.125       0 0.6487194
      6     gam -1.04822764      0.150       0 0.6310149
      7     gam -1.02111683      0.175       1 0.6246845
      8     gam -1.02084053      0.200       0 0.6246197
      9     gam -0.92231184      0.225       1 0.6012645
      10    gam -0.90794320      0.250       1 0.5978169
      11    gam -0.84852363      0.275       0 0.5834604
      12    gam -0.84606645      0.300       0 0.5828634
      13    gam -0.84226992      0.325       1 0.5819407
      14    gam -0.78613501      0.350       1 0.5682330
      15    gam -0.76480480      0.375       1 0.5629956
      16    gam -0.71962806      0.400       1 0.5518578
      17    gam -0.71786211      0.425       0 0.5514213
      18    gam -0.64951309      0.450       1 0.5344723
      19    gam -0.59036428      0.475       0 0.5197388
      20    gam -0.54408209      0.500       1 0.5081849
      21    gam -0.53022892      0.525       1 0.5047243
      22    gam -0.42893547      0.550       0 0.4794282
      23    gam -0.37097378      0.575       0 0.4649923
      24    gam -0.34696534      0.600       0 0.4590285
      25    gam -0.34292989      0.625       1 0.4580272
      26    gam -0.29597482      0.650       1 0.4464032
      27    gam -0.25651524      0.675       1 0.4366793
      28    gam -0.22782199      0.700       1 0.4296386
      29    gam -0.21288369      0.725       1 0.4259840
      30    gam -0.20302153      0.750       0 0.4235757
      31    gam -0.16686510      0.775       0 0.4147783
      32    gam -0.09726820      0.800       0 0.3980008
      33    gam -0.01090609      0.825       0 0.3775141
      34    gam  0.04228831      0.850       0 0.3651047
      35    gam  0.12694900      0.875       1 0.3457244
      36    gam  0.21579546      0.900       0 0.3259235
      37    gam  0.26854024      0.925       0 0.3144504
      38    gam  0.28965963      0.950       0 0.3099183
      39    gam  0.31936674      0.975       1 0.3036047
      40    gam  0.70851973      1.000       0 0.2280920

---

    Code
      out3$citl
    Output
        outcome      score percentile                   method
      1     0.5 -0.5098193      0.525 Calibration In The Large

---

    Code
      as.data.frame(out6$data)
    Output
           method       score percentile outcome  estimate
      1    BINNED -0.39652158      0.100      NA 0.2500000
      2    BINNED -0.09353167      0.200      NA 0.2500000
      3    BINNED  0.17000963      0.300      NA 0.2500000
      4    BINNED  0.28081049      0.400      NA 1.0000000
      5    BINNED  0.41927588      0.500      NA 0.2500000
      6    BINNED  0.62545539      0.600      NA 0.5000000
      7    BINNED  0.77820945      0.700      NA 1.0000000
      8    BINNED  0.88121128      0.800      NA 0.5000000
      9    BINNED  1.05382560      0.900      NA 0.2500000
      10   BINNED  1.37944817      1.000      NA 0.7500000
      11     PAVA -0.70851973      0.025       0 0.0000000
      12     PAVA -0.31936674      0.050       1 0.2000000
      13     PAVA -0.28965963      0.075       0 0.2000000
      14     PAVA -0.26854024      0.100       0 0.2000000
      15     PAVA -0.21579546      0.125       0 0.2000000
      16     PAVA -0.12694900      0.150       1 0.2000000
      17     PAVA -0.04228831      0.175       0 0.2000000
      18     PAVA  0.01090609      0.200       0 0.2000000
      19     PAVA  0.09726820      0.225       0 0.2000000
      20     PAVA  0.16686510      0.250       0 0.2000000
      21     PAVA  0.20302153      0.275       0 0.2000000
      22     PAVA  0.21288369      0.300       1 0.5925926
      23     PAVA  0.22782199      0.325       1 0.5925926
      24     PAVA  0.25651524      0.350       1 0.5925926
      25     PAVA  0.29597482      0.375       1 0.5925926
      26     PAVA  0.34292989      0.400       1 0.5925926
      27     PAVA  0.34696534      0.425       0 0.5925926
      28     PAVA  0.37097378      0.450       0 0.5925926
      29     PAVA  0.42893547      0.475       0 0.5925926
      30     PAVA  0.53022892      0.500       1 0.5925926
      31     PAVA  0.54408209      0.525       1 0.5925926
      32     PAVA  0.59036428      0.550       0 0.5925926
      33     PAVA  0.64951309      0.575       1 0.5925926
      34     PAVA  0.71786211      0.600       0 0.5925926
      35     PAVA  0.71962806      0.625       1 0.5925926
      36     PAVA  0.76480480      0.650       1 0.5925926
      37     PAVA  0.78613501      0.675       1 0.5925926
      38     PAVA  0.84226992      0.700       1 0.5925926
      39     PAVA  0.84606645      0.725       0 0.5925926
      40     PAVA  0.84852363      0.750       0 0.5925926
      41     PAVA  0.90794320      0.775       1 0.5925926
      42     PAVA  0.92231184      0.800       1 0.5925926
      43     PAVA  1.02084053      0.825       0 0.5925926
      44     PAVA  1.02111683      0.850       1 0.5925926
      45     PAVA  1.04822764      0.875       0 0.5925926
      46     PAVA  1.12511740      0.900       0 0.5925926
      47     PAVA  1.27747068      0.925       1 0.5925926
      48     PAVA  1.32986226      0.950       0 0.5925926
      49     PAVA  1.35878197      0.975       1 1.0000000
      50     PAVA  1.55167777      1.000       1 1.0000000
      51  MSPLINE -0.70851973      0.025       0 0.3134146
      52  MSPLINE -0.31936674      0.050       1 0.3229831
      53  MSPLINE -0.28965963      0.075       0 0.3325516
      54  MSPLINE -0.26854024      0.100       0 0.3421201
      55  MSPLINE -0.21579546      0.125       0 0.3516886
      56  MSPLINE -0.12694900      0.150       1 0.3612570
      57  MSPLINE -0.04228831      0.175       0 0.3708255
      58  MSPLINE  0.01090609      0.200       0 0.3803940
      59  MSPLINE  0.09726820      0.225       0 0.3899625
      60  MSPLINE  0.16686510      0.250       0 0.3995310
      61  MSPLINE  0.20302153      0.275       0 0.4090994
      62  MSPLINE  0.21288369      0.300       1 0.4186679
      63  MSPLINE  0.22782199      0.325       1 0.4282364
      64  MSPLINE  0.25651524      0.350       1 0.4378049
      65  MSPLINE  0.29597482      0.375       1 0.4473734
      66  MSPLINE  0.34292989      0.400       1 0.4569418
      67  MSPLINE  0.34696534      0.425       0 0.4665103
      68  MSPLINE  0.37097378      0.450       0 0.4760788
      69  MSPLINE  0.42893547      0.475       0 0.4856473
      70  MSPLINE  0.53022892      0.500       1 0.4952158
      71  MSPLINE  0.54408209      0.525       1 0.5047842
      72  MSPLINE  0.59036428      0.550       0 0.5143527
      73  MSPLINE  0.64951309      0.575       1 0.5239212
      74  MSPLINE  0.71786211      0.600       0 0.5334897
      75  MSPLINE  0.71962806      0.625       1 0.5430582
      76  MSPLINE  0.76480480      0.650       1 0.5526266
      77  MSPLINE  0.78613501      0.675       1 0.5621951
      78  MSPLINE  0.84226992      0.700       1 0.5717636
      79  MSPLINE  0.84606645      0.725       0 0.5813321
      80  MSPLINE  0.84852363      0.750       0 0.5909006
      81  MSPLINE  0.90794320      0.775       1 0.6004690
      82  MSPLINE  0.92231184      0.800       1 0.6100375
      83  MSPLINE  1.02084053      0.825       0 0.6196060
      84  MSPLINE  1.02111683      0.850       1 0.6291745
      85  MSPLINE  1.04822764      0.875       0 0.6387430
      86  MSPLINE  1.12511740      0.900       0 0.6483114
      87  MSPLINE  1.27747068      0.925       1 0.6578799
      88  MSPLINE  1.32986226      0.950       0 0.6674484
      89  MSPLINE  1.35878197      0.975       1 0.6770169
      90  MSPLINE  1.55167777      1.000       1 0.6865854
      91      gam -0.70851973      0.025       0 0.3166256
      92      gam -0.31936674      0.050       1 0.3252244
      93      gam -0.28965963      0.075       0 0.3339427
      94      gam -0.26854024      0.100       0 0.3427759
      95      gam -0.21579546      0.125       0 0.3517193
      96      gam -0.12694900      0.150       1 0.3607680
      97      gam -0.04228831      0.175       0 0.3699167
      98      gam  0.01090609      0.200       0 0.3791597
      99      gam  0.09726820      0.225       0 0.3884912
      100     gam  0.16686510      0.250       0 0.3979052
      101     gam  0.20302153      0.275       0 0.4073952
      102     gam  0.21288369      0.300       1 0.4169546
      103     gam  0.22782199      0.325       1 0.4265767
      104     gam  0.25651524      0.350       1 0.4362544
      105     gam  0.29597482      0.375       1 0.4459807
      106     gam  0.34292989      0.400       1 0.4557483
      107     gam  0.34696534      0.425       0 0.4655498
      108     gam  0.37097378      0.450       0 0.4753776
      109     gam  0.42893547      0.475       0 0.4852244
      110     gam  0.53022892      0.500       1 0.4950824
      111     gam  0.54408209      0.525       1 0.5049440
      112     gam  0.59036428      0.550       0 0.5148015
      113     gam  0.64951309      0.575       1 0.5246473
      114     gam  0.71786211      0.600       0 0.5344737
      115     gam  0.71962806      0.625       1 0.5442731
      116     gam  0.76480480      0.650       1 0.5540382
      117     gam  0.78613501      0.675       1 0.5637615
      118     gam  0.84226992      0.700       1 0.5734359
      119     gam  0.84606645      0.725       0 0.5830542
      120     gam  0.84852363      0.750       0 0.5926096
      121     gam  0.90794320      0.775       1 0.6020954
      122     gam  0.92231184      0.800       1 0.6115050
      123     gam  1.02084053      0.825       0 0.6208322
      124     gam  1.02111683      0.850       1 0.6300709
      125     gam  1.04822764      0.875       0 0.6392153
      126     gam  1.12511740      0.900       0 0.6482599
      127     gam  1.27747068      0.925       1 0.6571993
      128     gam  1.32986226      0.950       0 0.6660286
      129     gam  1.35878197      0.975       1 0.6747431
      130     gam  1.55167777      1.000       1 0.6833384

---

    Code
      out6$citl
    Output
        outcome     score percentile                   method
      1     0.5 0.5098193      0.475 Calibration In The Large

---

    Code
      as.data.frame(out7$data)
    Output
         method       score percentile outcome  estimate
      1     gam -0.70851973      0.025       0 0.3166256
      2     gam -0.31936674      0.050       1 0.3252244
      3     gam -0.28965963      0.075       0 0.3339427
      4     gam -0.26854024      0.100       0 0.3427759
      5     gam -0.21579546      0.125       0 0.3517193
      6     gam -0.12694900      0.150       1 0.3607680
      7     gam -0.04228831      0.175       0 0.3699167
      8     gam  0.01090609      0.200       0 0.3791597
      9     gam  0.09726820      0.225       0 0.3884912
      10    gam  0.16686510      0.250       0 0.3979052
      11    gam  0.20302153      0.275       0 0.4073952
      12    gam  0.21288369      0.300       1 0.4169546
      13    gam  0.22782199      0.325       1 0.4265767
      14    gam  0.25651524      0.350       1 0.4362544
      15    gam  0.29597482      0.375       1 0.4459807
      16    gam  0.34292989      0.400       1 0.4557483
      17    gam  0.34696534      0.425       0 0.4655498
      18    gam  0.37097378      0.450       0 0.4753776
      19    gam  0.42893547      0.475       0 0.4852244
      20    gam  0.53022892      0.500       1 0.4950824
      21    gam  0.54408209      0.525       1 0.5049440
      22    gam  0.59036428      0.550       0 0.5148015
      23    gam  0.64951309      0.575       1 0.5246473
      24    gam  0.71786211      0.600       0 0.5344737
      25    gam  0.71962806      0.625       1 0.5442731
      26    gam  0.76480480      0.650       1 0.5540382
      27    gam  0.78613501      0.675       1 0.5637615
      28    gam  0.84226992      0.700       1 0.5734359
      29    gam  0.84606645      0.725       0 0.5830542
      30    gam  0.84852363      0.750       0 0.5926096
      31    gam  0.90794320      0.775       1 0.6020954
      32    gam  0.92231184      0.800       1 0.6115050
      33    gam  1.02084053      0.825       0 0.6208322
      34    gam  1.02111683      0.850       1 0.6300709
      35    gam  1.04822764      0.875       0 0.6392153
      36    gam  1.12511740      0.900       0 0.6482599
      37    gam  1.27747068      0.925       1 0.6571993
      38    gam  1.32986226      0.950       0 0.6660286
      39    gam  1.35878197      0.975       1 0.6747431
      40    gam  1.55167777      1.000       1 0.6833384

---

    Code
      out7$citl
    Output
        outcome     score percentile                   method
      1     0.5 0.5098193      0.475 Calibration In The Large
