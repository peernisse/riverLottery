??
??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
h
ConcatV2
values"T*N
axis"Tidx
output"T"
Nint(0"	
Ttype"
Tidxtype0:
2	
8
Const
output"dtype"
valuetensor"
dtypetype
W

ExpandDims

input"T
dim"Tdim
output"T"	
Ttype"
Tdimtype0:
2	
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype?
>
RealDiv
x"T
y"T
z"T"
Ttype:
2	
E
Relu
features"T
activations"T"
Ttype:
2	
[
Reshape
tensor"T
shape"Tshape
output"T"	
Ttype"
Tshapetype0:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ?
@
StaticRegexFullMatch	
input

output
"
patternstring
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
;
Sub
x"T
y"T
z"T"
Ttype:
2	
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?"serve*2.4.02v2.4.0-rc4-71-g582c8d236cb8??
x
dense_5/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@*
shared_namedense_5/kernel
q
"dense_5/kernel/Read/ReadVariableOpReadVariableOpdense_5/kernel*
_output_shapes

:@*
dtype0
p
dense_5/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*
shared_namedense_5/bias
i
 dense_5/bias/Read/ReadVariableOpReadVariableOpdense_5/bias*
_output_shapes
:@*
dtype0
x
dense_4/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@@*
shared_namedense_4/kernel
q
"dense_4/kernel/Read/ReadVariableOpReadVariableOpdense_4/kernel*
_output_shapes

:@@*
dtype0
p
dense_4/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*
shared_namedense_4/bias
i
 dense_4/bias/Read/ReadVariableOpReadVariableOpdense_4/bias*
_output_shapes
:@*
dtype0
x
dense_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@*
shared_namedense_3/kernel
q
"dense_3/kernel/Read/ReadVariableOpReadVariableOpdense_3/kernel*
_output_shapes

:@*
dtype0
p
dense_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_3/bias
i
 dense_3/bias/Read/ReadVariableOpReadVariableOpdense_3/bias*
_output_shapes
:*
dtype0
l
RMSprop/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_nameRMSprop/iter
e
 RMSprop/iter/Read/ReadVariableOpReadVariableOpRMSprop/iter*
_output_shapes
: *
dtype0	
n
RMSprop/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/decay
g
!RMSprop/decay/Read/ReadVariableOpReadVariableOpRMSprop/decay*
_output_shapes
: *
dtype0
~
RMSprop/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *&
shared_nameRMSprop/learning_rate
w
)RMSprop/learning_rate/Read/ReadVariableOpReadVariableOpRMSprop/learning_rate*
_output_shapes
: *
dtype0
t
RMSprop/momentumVarHandleOp*
_output_shapes
: *
dtype0*
shape: *!
shared_nameRMSprop/momentum
m
$RMSprop/momentum/Read/ReadVariableOpReadVariableOpRMSprop/momentum*
_output_shapes
: *
dtype0
j
RMSprop/rhoVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/rho
c
RMSprop/rho/Read/ReadVariableOpReadVariableOpRMSprop/rho*
_output_shapes
: *
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
?
RMSprop/dense_5/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@*+
shared_nameRMSprop/dense_5/kernel/rms
?
.RMSprop/dense_5/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_5/kernel/rms*
_output_shapes

:@*
dtype0
?
RMSprop/dense_5/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*)
shared_nameRMSprop/dense_5/bias/rms
?
,RMSprop/dense_5/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_5/bias/rms*
_output_shapes
:@*
dtype0
?
RMSprop/dense_4/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@@*+
shared_nameRMSprop/dense_4/kernel/rms
?
.RMSprop/dense_4/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_4/kernel/rms*
_output_shapes

:@@*
dtype0
?
RMSprop/dense_4/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*)
shared_nameRMSprop/dense_4/bias/rms
?
,RMSprop/dense_4/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_4/bias/rms*
_output_shapes
:@*
dtype0
?
RMSprop/dense_3/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@*+
shared_nameRMSprop/dense_3/kernel/rms
?
.RMSprop/dense_3/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_3/kernel/rms*
_output_shapes

:@*
dtype0
?
RMSprop/dense_3/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:*)
shared_nameRMSprop/dense_3/bias/rms
?
,RMSprop/dense_3/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_3/bias/rms*
_output_shapes
:*
dtype0

NoOpNoOp
?#
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*?"
value?"B?" B?"
?
layer-0
layer-1
layer-2
layer-3
layer-4
layer-5
layer-6
layer_with_weights-0
layer-7
	layer_with_weights-1
	layer-8

layer_with_weights-2

layer-9
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
 
 
 
 
 
 
x
_feature_columns

_resources
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
 trainable_variables
!regularization_losses
"	keras_api
h

#kernel
$bias
%	variables
&trainable_variables
'regularization_losses
(	keras_api
?
)iter
	*decay
+learning_rate
,momentum
-rho	rmsR	rmsS	rmsT	rmsU	#rmsV	$rmsW
*
0
1
2
3
#4
$5
*
0
1
2
3
#4
$5
 
?
.layer_metrics
	variables
trainable_variables
/layer_regularization_losses
0metrics
1non_trainable_variables
regularization_losses

2layers
 
 
 
 
 
 
?
3layer_metrics
	variables
trainable_variables
4layer_regularization_losses
5metrics
regularization_losses
6non_trainable_variables

7layers
ZX
VARIABLE_VALUEdense_5/kernel6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUEdense_5/bias4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
8layer_metrics
	variables
trainable_variables
9layer_regularization_losses
:metrics
regularization_losses
;non_trainable_variables

<layers
ZX
VARIABLE_VALUEdense_4/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUEdense_4/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
=layer_metrics
	variables
 trainable_variables
>layer_regularization_losses
?metrics
!regularization_losses
@non_trainable_variables

Alayers
ZX
VARIABLE_VALUEdense_3/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUEdense_3/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE

#0
$1

#0
$1
 
?
Blayer_metrics
%	variables
&trainable_variables
Clayer_regularization_losses
Dmetrics
'regularization_losses
Enon_trainable_variables

Flayers
KI
VARIABLE_VALUERMSprop/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
MK
VARIABLE_VALUERMSprop/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUERMSprop/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUERMSprop/momentum-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUE
IG
VARIABLE_VALUERMSprop/rho(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUE
 
 

G0
H1
 
F
0
1
2
3
4
5
6
7
	8

9
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
4
	Itotal
	Jcount
K	variables
L	keras_api
D
	Mtotal
	Ncount
O
_fn_kwargs
P	variables
Q	keras_api
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

I0
J1

K	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

M0
N1

P	variables
??
VARIABLE_VALUERMSprop/dense_5/kernel/rmsTlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?~
VARIABLE_VALUERMSprop/dense_5/bias/rmsRlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUERMSprop/dense_4/kernel/rmsTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?~
VARIABLE_VALUERMSprop/dense_4/bias/rmsRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUERMSprop/dense_3/kernel/rmsTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?~
VARIABLE_VALUERMSprop/dense_3/bias/rmsRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
t
serving_default_AVAILABLEPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
r
serving_default_GROUP_NPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
}
"serving_default_TOTAL_APPLICATIONSPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
q
serving_default_WDAY_NPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
o
serving_default_WEEKPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
o
serving_default_YEARPlaceholder*#
_output_shapes
:?????????*
dtype0*
shape:?????????
?
StatefulPartitionedCallStatefulPartitionedCallserving_default_AVAILABLEserving_default_GROUP_N"serving_default_TOTAL_APPLICATIONSserving_default_WDAY_Nserving_default_WEEKserving_default_YEARdense_5/kerneldense_5/biasdense_4/kerneldense_4/biasdense_3/kerneldense_3/bias*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *,
f'R%
#__inference_signature_wrapper_23112
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename"dense_5/kernel/Read/ReadVariableOp dense_5/bias/Read/ReadVariableOp"dense_4/kernel/Read/ReadVariableOp dense_4/bias/Read/ReadVariableOp"dense_3/kernel/Read/ReadVariableOp dense_3/bias/Read/ReadVariableOp RMSprop/iter/Read/ReadVariableOp!RMSprop/decay/Read/ReadVariableOp)RMSprop/learning_rate/Read/ReadVariableOp$RMSprop/momentum/Read/ReadVariableOpRMSprop/rho/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOp.RMSprop/dense_5/kernel/rms/Read/ReadVariableOp,RMSprop/dense_5/bias/rms/Read/ReadVariableOp.RMSprop/dense_4/kernel/rms/Read/ReadVariableOp,RMSprop/dense_4/bias/rms/Read/ReadVariableOp.RMSprop/dense_3/kernel/rms/Read/ReadVariableOp,RMSprop/dense_3/bias/rms/Read/ReadVariableOpConst*"
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__traced_save_23746
?
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense_5/kerneldense_5/biasdense_4/kerneldense_4/biasdense_3/kerneldense_3/biasRMSprop/iterRMSprop/decayRMSprop/learning_rateRMSprop/momentumRMSprop/rhototalcounttotal_1count_1RMSprop/dense_5/kernel/rmsRMSprop/dense_5/bias/rmsRMSprop/dense_4/kernel/rmsRMSprop/dense_4/bias/rmsRMSprop/dense_3/kernel/rmsRMSprop/dense_3/bias/rms*!
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? **
f%R#
!__inference__traced_restore_23819??

?
?
B__inference_model_1_layer_call_and_return_conditional_losses_22960
	available
group_n
total_applications

wday_n
week
year
dense_5_22901
dense_5_22903
dense_4_22928
dense_4_22930
dense_3_22954
dense_3_22956
identity??dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?
 dense_features_1/PartitionedCallPartitionedCall	availablegroup_ntotal_applicationswday_nweekyear*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_227612"
 dense_features_1/PartitionedCall?
dense_5/StatefulPartitionedCallStatefulPartitionedCall)dense_features_1/PartitionedCall:output:0dense_5_22901dense_5_22903*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_5_layer_call_and_return_conditional_losses_228902!
dense_5/StatefulPartitionedCall?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dense_5/StatefulPartitionedCall:output:0dense_4_22928dense_4_22930*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_4_layer_call_and_return_conditional_losses_229172!
dense_4/StatefulPartitionedCall?
dense_3/StatefulPartitionedCallStatefulPartitionedCall(dense_4/StatefulPartitionedCall:output:0dense_3_22954dense_3_22956*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_229432!
dense_3/StatefulPartitionedCall?
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0 ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?2
?
__inference__traced_save_23746
file_prefix-
)savev2_dense_5_kernel_read_readvariableop+
'savev2_dense_5_bias_read_readvariableop-
)savev2_dense_4_kernel_read_readvariableop+
'savev2_dense_4_bias_read_readvariableop-
)savev2_dense_3_kernel_read_readvariableop+
'savev2_dense_3_bias_read_readvariableop+
'savev2_rmsprop_iter_read_readvariableop	,
(savev2_rmsprop_decay_read_readvariableop4
0savev2_rmsprop_learning_rate_read_readvariableop/
+savev2_rmsprop_momentum_read_readvariableop*
&savev2_rmsprop_rho_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop9
5savev2_rmsprop_dense_5_kernel_rms_read_readvariableop7
3savev2_rmsprop_dense_5_bias_rms_read_readvariableop9
5savev2_rmsprop_dense_4_kernel_rms_read_readvariableop7
3savev2_rmsprop_dense_4_bias_rms_read_readvariableop9
5savev2_rmsprop_dense_3_kernel_rms_read_readvariableop7
3savev2_rmsprop_dense_3_bias_rms_read_readvariableop
savev2_const

identity_1??MergeV2Checkpoints?
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Constl
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part2	
Const_1?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?

value?
B?
B6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value6B4B B B B B B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0)savev2_dense_5_kernel_read_readvariableop'savev2_dense_5_bias_read_readvariableop)savev2_dense_4_kernel_read_readvariableop'savev2_dense_4_bias_read_readvariableop)savev2_dense_3_kernel_read_readvariableop'savev2_dense_3_bias_read_readvariableop'savev2_rmsprop_iter_read_readvariableop(savev2_rmsprop_decay_read_readvariableop0savev2_rmsprop_learning_rate_read_readvariableop+savev2_rmsprop_momentum_read_readvariableop&savev2_rmsprop_rho_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop5savev2_rmsprop_dense_5_kernel_rms_read_readvariableop3savev2_rmsprop_dense_5_bias_rms_read_readvariableop5savev2_rmsprop_dense_4_kernel_rms_read_readvariableop3savev2_rmsprop_dense_4_bias_rms_read_readvariableop5savev2_rmsprop_dense_3_kernel_rms_read_readvariableop3savev2_rmsprop_dense_3_bias_rms_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *$
dtypes
2	2
SaveV2?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identitym

Identity_1IdentityIdentity:output:0^MergeV2Checkpoints*
T0*
_output_shapes
: 2

Identity_1"!

identity_1Identity_1:output:0*?
_input_shapesx
v: :@:@:@@:@:@:: : : : : : : : : :@:@:@@:@:@:: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

:@: 

_output_shapes
:@:$ 

_output_shapes

:@@: 

_output_shapes
:@:$ 

_output_shapes

:@: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: :


_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:@: 

_output_shapes
:@:$ 

_output_shapes

:@@: 

_output_shapes
:@:$ 

_output_shapes

:@: 

_output_shapes
::

_output_shapes
: 
?Z
?

!__inference__traced_restore_23819
file_prefix#
assignvariableop_dense_5_kernel#
assignvariableop_1_dense_5_bias%
!assignvariableop_2_dense_4_kernel#
assignvariableop_3_dense_4_bias%
!assignvariableop_4_dense_3_kernel#
assignvariableop_5_dense_3_bias#
assignvariableop_6_rmsprop_iter$
 assignvariableop_7_rmsprop_decay,
(assignvariableop_8_rmsprop_learning_rate'
#assignvariableop_9_rmsprop_momentum#
assignvariableop_10_rmsprop_rho
assignvariableop_11_total
assignvariableop_12_count
assignvariableop_13_total_1
assignvariableop_14_count_12
.assignvariableop_15_rmsprop_dense_5_kernel_rms0
,assignvariableop_16_rmsprop_dense_5_bias_rms2
.assignvariableop_17_rmsprop_dense_4_kernel_rms0
,assignvariableop_18_rmsprop_dense_4_bias_rms2
.assignvariableop_19_rmsprop_dense_3_kernel_rms0
,assignvariableop_20_rmsprop_dense_3_bias_rms
identity_22??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_3?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?

value?
B?
B6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value6B4B B B B B B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*l
_output_shapesZ
X::::::::::::::::::::::*$
dtypes
2	2
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:2

Identity?
AssignVariableOpAssignVariableOpassignvariableop_dense_5_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1?
AssignVariableOp_1AssignVariableOpassignvariableop_1_dense_5_biasIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2?
AssignVariableOp_2AssignVariableOp!assignvariableop_2_dense_4_kernelIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3?
AssignVariableOp_3AssignVariableOpassignvariableop_3_dense_4_biasIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4?
AssignVariableOp_4AssignVariableOp!assignvariableop_4_dense_3_kernelIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5?
AssignVariableOp_5AssignVariableOpassignvariableop_5_dense_3_biasIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0	*
_output_shapes
:2

Identity_6?
AssignVariableOp_6AssignVariableOpassignvariableop_6_rmsprop_iterIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	2
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7?
AssignVariableOp_7AssignVariableOp assignvariableop_7_rmsprop_decayIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8?
AssignVariableOp_8AssignVariableOp(assignvariableop_8_rmsprop_learning_rateIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9?
AssignVariableOp_9AssignVariableOp#assignvariableop_9_rmsprop_momentumIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10?
AssignVariableOp_10AssignVariableOpassignvariableop_10_rmsprop_rhoIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11?
AssignVariableOp_11AssignVariableOpassignvariableop_11_totalIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12?
AssignVariableOp_12AssignVariableOpassignvariableop_12_countIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13?
AssignVariableOp_13AssignVariableOpassignvariableop_13_total_1Identity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14?
AssignVariableOp_14AssignVariableOpassignvariableop_14_count_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15?
AssignVariableOp_15AssignVariableOp.assignvariableop_15_rmsprop_dense_5_kernel_rmsIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_15n
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:2
Identity_16?
AssignVariableOp_16AssignVariableOp,assignvariableop_16_rmsprop_dense_5_bias_rmsIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_16n
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:2
Identity_17?
AssignVariableOp_17AssignVariableOp.assignvariableop_17_rmsprop_dense_4_kernel_rmsIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_17n
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:2
Identity_18?
AssignVariableOp_18AssignVariableOp,assignvariableop_18_rmsprop_dense_4_bias_rmsIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_18n
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:2
Identity_19?
AssignVariableOp_19AssignVariableOp.assignvariableop_19_rmsprop_dense_3_kernel_rmsIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_19n
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:2
Identity_20?
AssignVariableOp_20AssignVariableOp,assignvariableop_20_rmsprop_dense_3_bias_rmsIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_209
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp?
Identity_21Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_21?
Identity_22IdentityIdentity_21:output:0^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*
T0*
_output_shapes
: 2
Identity_22"#
identity_22Identity_22:output:0*i
_input_shapesX
V: :::::::::::::::::::::2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
?j
?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23481
features_available
features_group_n
features_total_applications
features_wday_n
features_week
features_year
identity
AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
AVAILABLE/ExpandDims/dim?
AVAILABLE/ExpandDims
ExpandDimsfeatures_available!AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/ExpandDimsi
AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2
AVAILABLE/Cast/x?
AVAILABLE/SubSubAVAILABLE/ExpandDims:output:0AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Subm
AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2
AVAILABLE/Cast_1/x?
AVAILABLE/truedivRealDivAVAILABLE/Sub:z:0AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/truedivg
AVAILABLE/ShapeShapeAVAILABLE/truediv:z:0*
T0*
_output_shapes
:2
AVAILABLE/Shape?
AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
AVAILABLE/strided_slice/stack?
AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_1?
AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_2?
AVAILABLE/strided_sliceStridedSliceAVAILABLE/Shape:output:0&AVAILABLE/strided_slice/stack:output:0(AVAILABLE/strided_slice/stack_1:output:0(AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
AVAILABLE/strided_slicex
AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
AVAILABLE/Reshape/shape/1?
AVAILABLE/Reshape/shapePack AVAILABLE/strided_slice:output:0"AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
AVAILABLE/Reshape/shape?
AVAILABLE/ReshapeReshapeAVAILABLE/truediv:z:0 AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Reshape{
GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
GROUP_N/ExpandDims/dim?
GROUP_N/ExpandDims
ExpandDimsfeatures_group_nGROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/ExpandDimse
GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2
GROUP_N/Cast/x?
GROUP_N/SubSubGROUP_N/ExpandDims:output:0GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Subi
GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2
GROUP_N/Cast_1/x?
GROUP_N/truedivRealDivGROUP_N/Sub:z:0GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/truediva
GROUP_N/ShapeShapeGROUP_N/truediv:z:0*
T0*
_output_shapes
:2
GROUP_N/Shape?
GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
GROUP_N/strided_slice/stack?
GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_1?
GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_2?
GROUP_N/strided_sliceStridedSliceGROUP_N/Shape:output:0$GROUP_N/strided_slice/stack:output:0&GROUP_N/strided_slice/stack_1:output:0&GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
GROUP_N/strided_slicet
GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
GROUP_N/Reshape/shape/1?
GROUP_N/Reshape/shapePackGROUP_N/strided_slice:output:0 GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
GROUP_N/Reshape/shape?
GROUP_N/ReshapeReshapeGROUP_N/truediv:z:0GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Reshape?
!TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2#
!TOTAL_APPLICATIONS/ExpandDims/dim?
TOTAL_APPLICATIONS/ExpandDims
ExpandDimsfeatures_total_applications*TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/ExpandDims{
TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2
TOTAL_APPLICATIONS/Cast/x?
TOTAL_APPLICATIONS/SubSub&TOTAL_APPLICATIONS/ExpandDims:output:0"TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Sub
TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2
TOTAL_APPLICATIONS/Cast_1/x?
TOTAL_APPLICATIONS/truedivRealDivTOTAL_APPLICATIONS/Sub:z:0$TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/truediv?
TOTAL_APPLICATIONS/ShapeShapeTOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2
TOTAL_APPLICATIONS/Shape?
&TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&TOTAL_APPLICATIONS/strided_slice/stack?
(TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_1?
(TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_2?
 TOTAL_APPLICATIONS/strided_sliceStridedSlice!TOTAL_APPLICATIONS/Shape:output:0/TOTAL_APPLICATIONS/strided_slice/stack:output:01TOTAL_APPLICATIONS/strided_slice/stack_1:output:01TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 TOTAL_APPLICATIONS/strided_slice?
"TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2$
"TOTAL_APPLICATIONS/Reshape/shape/1?
 TOTAL_APPLICATIONS/Reshape/shapePack)TOTAL_APPLICATIONS/strided_slice:output:0+TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2"
 TOTAL_APPLICATIONS/Reshape/shape?
TOTAL_APPLICATIONS/ReshapeReshapeTOTAL_APPLICATIONS/truediv:z:0)TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Reshapey
WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WDAY_N/ExpandDims/dim?
WDAY_N/ExpandDims
ExpandDimsfeatures_wday_nWDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/ExpandDimsc
WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2
WDAY_N/Cast/x?

WDAY_N/SubSubWDAY_N/ExpandDims:output:0WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WDAY_N/Subg
WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2
WDAY_N/Cast_1/x?
WDAY_N/truedivRealDivWDAY_N/Sub:z:0WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/truediv^
WDAY_N/ShapeShapeWDAY_N/truediv:z:0*
T0*
_output_shapes
:2
WDAY_N/Shape?
WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WDAY_N/strided_slice/stack?
WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_1?
WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_2?
WDAY_N/strided_sliceStridedSliceWDAY_N/Shape:output:0#WDAY_N/strided_slice/stack:output:0%WDAY_N/strided_slice/stack_1:output:0%WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WDAY_N/strided_slicer
WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WDAY_N/Reshape/shape/1?
WDAY_N/Reshape/shapePackWDAY_N/strided_slice:output:0WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WDAY_N/Reshape/shape?
WDAY_N/ReshapeReshapeWDAY_N/truediv:z:0WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/Reshapeu
WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WEEK/ExpandDims/dim?
WEEK/ExpandDims
ExpandDimsfeatures_weekWEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WEEK/ExpandDims_
WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
WEEK/Cast/x}
WEEK/SubSubWEEK/ExpandDims:output:0WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WEEK/Subc
WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2
WEEK/Cast_1/x
WEEK/truedivRealDivWEEK/Sub:z:0WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WEEK/truedivX

WEEK/ShapeShapeWEEK/truediv:z:0*
T0*
_output_shapes
:2

WEEK/Shape~
WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WEEK/strided_slice/stack?
WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_1?
WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_2?
WEEK/strided_sliceStridedSliceWEEK/Shape:output:0!WEEK/strided_slice/stack:output:0#WEEK/strided_slice/stack_1:output:0#WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WEEK/strided_slicen
WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WEEK/Reshape/shape/1?
WEEK/Reshape/shapePackWEEK/strided_slice:output:0WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WEEK/Reshape/shape?
WEEK/ReshapeReshapeWEEK/truediv:z:0WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WEEK/Reshapeu
YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
YEAR/ExpandDims/dim?
YEAR/ExpandDims
ExpandDimsfeatures_yearYEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
YEAR/ExpandDims_
YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
YEAR/Cast/x}
YEAR/SubSubYEAR/ExpandDims:output:0YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

YEAR/Subc
YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2
YEAR/Cast_1/x
YEAR/truedivRealDivYEAR/Sub:z:0YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
YEAR/truedivX

YEAR/ShapeShapeYEAR/truediv:z:0*
T0*
_output_shapes
:2

YEAR/Shape~
YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
YEAR/strided_slice/stack?
YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_1?
YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_2?
YEAR/strided_sliceStridedSliceYEAR/Shape:output:0!YEAR/strided_slice/stack:output:0#YEAR/strided_slice/stack_1:output:0#YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
YEAR/strided_slicen
YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
YEAR/Reshape/shape/1?
YEAR/Reshape/shapePackYEAR/strided_slice:output:0YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
YEAR/Reshape/shape?
YEAR/ReshapeReshapeYEAR/truediv:z:0YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
YEAR/Reshapee
concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
concat/axis?
concatConcatV2AVAILABLE/Reshape:output:0GROUP_N/Reshape:output:0#TOTAL_APPLICATIONS/Reshape:output:0WDAY_N/Reshape:output:0WEEK/Reshape:output:0YEAR/Reshape:output:0concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
concatc
IdentityIdentityconcat:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:W S
#
_output_shapes
:?????????
,
_user_specified_namefeatures/AVAILABLE:UQ
#
_output_shapes
:?????????
*
_user_specified_namefeatures/GROUP_N:`\
#
_output_shapes
:?????????
5
_user_specified_namefeatures/TOTAL_APPLICATIONS:TP
#
_output_shapes
:?????????
)
_user_specified_namefeatures/WDAY_N:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/WEEK:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/YEAR
?
?
'__inference_model_1_layer_call_fn_23080
	available
group_n
total_applications

wday_n
week
year
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall	availablegroup_ntotal_applicationswday_nweekyearunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_model_1_layer_call_and_return_conditional_losses_230652
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::22
StatefulPartitionedCallStatefulPartitionedCall:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?
?
0__inference_dense_features_1_layer_call_fn_23596
features_available
features_group_n
features_total_applications
features_wday_n
features_week
features_year
identity?
PartitionedCallPartitionedCallfeatures_availablefeatures_group_nfeatures_total_applicationsfeatures_wday_nfeatures_weekfeatures_year*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_228562
PartitionedCalll
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:W S
#
_output_shapes
:?????????
,
_user_specified_namefeatures/AVAILABLE:UQ
#
_output_shapes
:?????????
*
_user_specified_namefeatures/GROUP_N:`\
#
_output_shapes
:?????????
5
_user_specified_namefeatures/TOTAL_APPLICATIONS:TP
#
_output_shapes
:?????????
)
_user_specified_namefeatures/WDAY_N:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/WEEK:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/YEAR
?
?
'__inference_model_1_layer_call_fn_23364
inputs_available
inputs_group_n
inputs_total_applications
inputs_wday_n
inputs_week
inputs_year
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_availableinputs_group_ninputs_total_applicationsinputs_wday_ninputs_weekinputs_yearunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_model_1_layer_call_and_return_conditional_losses_230182
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::22
StatefulPartitionedCallStatefulPartitionedCall:U Q
#
_output_shapes
:?????????
*
_user_specified_nameinputs/AVAILABLE:SO
#
_output_shapes
:?????????
(
_user_specified_nameinputs/GROUP_N:^Z
#
_output_shapes
:?????????
3
_user_specified_nameinputs/TOTAL_APPLICATIONS:RN
#
_output_shapes
:?????????
'
_user_specified_nameinputs/WDAY_N:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/WEEK:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/YEAR
?	
?
B__inference_dense_5_layer_call_and_return_conditional_losses_23607

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relu?
IdentityIdentityRelu:activations:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
B__inference_model_1_layer_call_and_return_conditional_losses_22985
	available
group_n
total_applications

wday_n
week
year
dense_5_22969
dense_5_22971
dense_4_22974
dense_4_22976
dense_3_22979
dense_3_22981
identity??dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?
 dense_features_1/PartitionedCallPartitionedCall	availablegroup_ntotal_applicationswday_nweekyear*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_228562"
 dense_features_1/PartitionedCall?
dense_5/StatefulPartitionedCallStatefulPartitionedCall)dense_features_1/PartitionedCall:output:0dense_5_22969dense_5_22971*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_5_layer_call_and_return_conditional_losses_228902!
dense_5/StatefulPartitionedCall?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dense_5/StatefulPartitionedCall:output:0dense_4_22974dense_4_22976*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_4_layer_call_and_return_conditional_losses_229172!
dense_4/StatefulPartitionedCall?
dense_3/StatefulPartitionedCallStatefulPartitionedCall(dense_4/StatefulPartitionedCall:output:0dense_3_22979dense_3_22981*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_229432!
dense_3/StatefulPartitionedCall?
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0 ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?
?
'__inference_model_1_layer_call_fn_23033
	available
group_n
total_applications

wday_n
week
year
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall	availablegroup_ntotal_applicationswday_nweekyearunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_model_1_layer_call_and_return_conditional_losses_230182
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::22
StatefulPartitionedCallStatefulPartitionedCall:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?
?
'__inference_model_1_layer_call_fn_23386
inputs_available
inputs_group_n
inputs_total_applications
inputs_wday_n
inputs_week
inputs_year
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_availableinputs_group_ninputs_total_applicationsinputs_wday_ninputs_weekinputs_yearunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_model_1_layer_call_and_return_conditional_losses_230652
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::22
StatefulPartitionedCallStatefulPartitionedCall:U Q
#
_output_shapes
:?????????
*
_user_specified_nameinputs/AVAILABLE:SO
#
_output_shapes
:?????????
(
_user_specified_nameinputs/GROUP_N:^Z
#
_output_shapes
:?????????
3
_user_specified_nameinputs/TOTAL_APPLICATIONS:RN
#
_output_shapes
:?????????
'
_user_specified_nameinputs/WDAY_N:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/WEEK:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/YEAR
?
|
'__inference_dense_3_layer_call_fn_23655

inputs
unknown
	unknown_0
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_229432
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?	
?
B__inference_dense_5_layer_call_and_return_conditional_losses_22890

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relu?
IdentityIdentityRelu:activations:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
??
?
 __inference__wrapped_model_22657
	available
group_n
total_applications

wday_n
week
year2
.model_1_dense_5_matmul_readvariableop_resource3
/model_1_dense_5_biasadd_readvariableop_resource2
.model_1_dense_4_matmul_readvariableop_resource3
/model_1_dense_4_biasadd_readvariableop_resource2
.model_1_dense_3_matmul_readvariableop_resource3
/model_1_dense_3_biasadd_readvariableop_resource
identity??&model_1/dense_3/BiasAdd/ReadVariableOp?%model_1/dense_3/MatMul/ReadVariableOp?&model_1/dense_4/BiasAdd/ReadVariableOp?%model_1/dense_4/MatMul/ReadVariableOp?&model_1/dense_5/BiasAdd/ReadVariableOp?%model_1/dense_5/MatMul/ReadVariableOp?
1model_1/dense_features_1/AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????23
1model_1/dense_features_1/AVAILABLE/ExpandDims/dim?
-model_1/dense_features_1/AVAILABLE/ExpandDims
ExpandDims	available:model_1/dense_features_1/AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2/
-model_1/dense_features_1/AVAILABLE/ExpandDims?
)model_1/dense_features_1/AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2+
)model_1/dense_features_1/AVAILABLE/Cast/x?
&model_1/dense_features_1/AVAILABLE/SubSub6model_1/dense_features_1/AVAILABLE/ExpandDims:output:02model_1/dense_features_1/AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2(
&model_1/dense_features_1/AVAILABLE/Sub?
+model_1/dense_features_1/AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2-
+model_1/dense_features_1/AVAILABLE/Cast_1/x?
*model_1/dense_features_1/AVAILABLE/truedivRealDiv*model_1/dense_features_1/AVAILABLE/Sub:z:04model_1/dense_features_1/AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2,
*model_1/dense_features_1/AVAILABLE/truediv?
(model_1/dense_features_1/AVAILABLE/ShapeShape.model_1/dense_features_1/AVAILABLE/truediv:z:0*
T0*
_output_shapes
:2*
(model_1/dense_features_1/AVAILABLE/Shape?
6model_1/dense_features_1/AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6model_1/dense_features_1/AVAILABLE/strided_slice/stack?
8model_1/dense_features_1/AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8model_1/dense_features_1/AVAILABLE/strided_slice/stack_1?
8model_1/dense_features_1/AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8model_1/dense_features_1/AVAILABLE/strided_slice/stack_2?
0model_1/dense_features_1/AVAILABLE/strided_sliceStridedSlice1model_1/dense_features_1/AVAILABLE/Shape:output:0?model_1/dense_features_1/AVAILABLE/strided_slice/stack:output:0Amodel_1/dense_features_1/AVAILABLE/strided_slice/stack_1:output:0Amodel_1/dense_features_1/AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask22
0model_1/dense_features_1/AVAILABLE/strided_slice?
2model_1/dense_features_1/AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :24
2model_1/dense_features_1/AVAILABLE/Reshape/shape/1?
0model_1/dense_features_1/AVAILABLE/Reshape/shapePack9model_1/dense_features_1/AVAILABLE/strided_slice:output:0;model_1/dense_features_1/AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:22
0model_1/dense_features_1/AVAILABLE/Reshape/shape?
*model_1/dense_features_1/AVAILABLE/ReshapeReshape.model_1/dense_features_1/AVAILABLE/truediv:z:09model_1/dense_features_1/AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2,
*model_1/dense_features_1/AVAILABLE/Reshape?
/model_1/dense_features_1/GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????21
/model_1/dense_features_1/GROUP_N/ExpandDims/dim?
+model_1/dense_features_1/GROUP_N/ExpandDims
ExpandDimsgroup_n8model_1/dense_features_1/GROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2-
+model_1/dense_features_1/GROUP_N/ExpandDims?
'model_1/dense_features_1/GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2)
'model_1/dense_features_1/GROUP_N/Cast/x?
$model_1/dense_features_1/GROUP_N/SubSub4model_1/dense_features_1/GROUP_N/ExpandDims:output:00model_1/dense_features_1/GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2&
$model_1/dense_features_1/GROUP_N/Sub?
)model_1/dense_features_1/GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2+
)model_1/dense_features_1/GROUP_N/Cast_1/x?
(model_1/dense_features_1/GROUP_N/truedivRealDiv(model_1/dense_features_1/GROUP_N/Sub:z:02model_1/dense_features_1/GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2*
(model_1/dense_features_1/GROUP_N/truediv?
&model_1/dense_features_1/GROUP_N/ShapeShape,model_1/dense_features_1/GROUP_N/truediv:z:0*
T0*
_output_shapes
:2(
&model_1/dense_features_1/GROUP_N/Shape?
4model_1/dense_features_1/GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 26
4model_1/dense_features_1/GROUP_N/strided_slice/stack?
6model_1/dense_features_1/GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:28
6model_1/dense_features_1/GROUP_N/strided_slice/stack_1?
6model_1/dense_features_1/GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:28
6model_1/dense_features_1/GROUP_N/strided_slice/stack_2?
.model_1/dense_features_1/GROUP_N/strided_sliceStridedSlice/model_1/dense_features_1/GROUP_N/Shape:output:0=model_1/dense_features_1/GROUP_N/strided_slice/stack:output:0?model_1/dense_features_1/GROUP_N/strided_slice/stack_1:output:0?model_1/dense_features_1/GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask20
.model_1/dense_features_1/GROUP_N/strided_slice?
0model_1/dense_features_1/GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :22
0model_1/dense_features_1/GROUP_N/Reshape/shape/1?
.model_1/dense_features_1/GROUP_N/Reshape/shapePack7model_1/dense_features_1/GROUP_N/strided_slice:output:09model_1/dense_features_1/GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:20
.model_1/dense_features_1/GROUP_N/Reshape/shape?
(model_1/dense_features_1/GROUP_N/ReshapeReshape,model_1/dense_features_1/GROUP_N/truediv:z:07model_1/dense_features_1/GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2*
(model_1/dense_features_1/GROUP_N/Reshape?
:model_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2<
:model_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim?
6model_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims
ExpandDimstotal_applicationsCmodel_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????28
6model_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims?
2model_1/dense_features_1/TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C24
2model_1/dense_features_1/TOTAL_APPLICATIONS/Cast/x?
/model_1/dense_features_1/TOTAL_APPLICATIONS/SubSub?model_1/dense_features_1/TOTAL_APPLICATIONS/ExpandDims:output:0;model_1/dense_features_1/TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????21
/model_1/dense_features_1/TOTAL_APPLICATIONS/Sub?
4model_1/dense_features_1/TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C26
4model_1/dense_features_1/TOTAL_APPLICATIONS/Cast_1/x?
3model_1/dense_features_1/TOTAL_APPLICATIONS/truedivRealDiv3model_1/dense_features_1/TOTAL_APPLICATIONS/Sub:z:0=model_1/dense_features_1/TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????25
3model_1/dense_features_1/TOTAL_APPLICATIONS/truediv?
1model_1/dense_features_1/TOTAL_APPLICATIONS/ShapeShape7model_1/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:23
1model_1/dense_features_1/TOTAL_APPLICATIONS/Shape?
?model_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2A
?model_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack?
Amodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2C
Amodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1?
Amodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2C
Amodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2?
9model_1/dense_features_1/TOTAL_APPLICATIONS/strided_sliceStridedSlice:model_1/dense_features_1/TOTAL_APPLICATIONS/Shape:output:0Hmodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack:output:0Jmodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1:output:0Jmodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2;
9model_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice?
;model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2=
;model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1?
9model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shapePackBmodel_1/dense_features_1/TOTAL_APPLICATIONS/strided_slice:output:0Dmodel_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2;
9model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shape?
3model_1/dense_features_1/TOTAL_APPLICATIONS/ReshapeReshape7model_1/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0Bmodel_1/dense_features_1/TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????25
3model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape?
.model_1/dense_features_1/WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????20
.model_1/dense_features_1/WDAY_N/ExpandDims/dim?
*model_1/dense_features_1/WDAY_N/ExpandDims
ExpandDimswday_n7model_1/dense_features_1/WDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2,
*model_1/dense_features_1/WDAY_N/ExpandDims?
&model_1/dense_features_1/WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2(
&model_1/dense_features_1/WDAY_N/Cast/x?
#model_1/dense_features_1/WDAY_N/SubSub3model_1/dense_features_1/WDAY_N/ExpandDims:output:0/model_1/dense_features_1/WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2%
#model_1/dense_features_1/WDAY_N/Sub?
(model_1/dense_features_1/WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2*
(model_1/dense_features_1/WDAY_N/Cast_1/x?
'model_1/dense_features_1/WDAY_N/truedivRealDiv'model_1/dense_features_1/WDAY_N/Sub:z:01model_1/dense_features_1/WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2)
'model_1/dense_features_1/WDAY_N/truediv?
%model_1/dense_features_1/WDAY_N/ShapeShape+model_1/dense_features_1/WDAY_N/truediv:z:0*
T0*
_output_shapes
:2'
%model_1/dense_features_1/WDAY_N/Shape?
3model_1/dense_features_1/WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 25
3model_1/dense_features_1/WDAY_N/strided_slice/stack?
5model_1/dense_features_1/WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:27
5model_1/dense_features_1/WDAY_N/strided_slice/stack_1?
5model_1/dense_features_1/WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:27
5model_1/dense_features_1/WDAY_N/strided_slice/stack_2?
-model_1/dense_features_1/WDAY_N/strided_sliceStridedSlice.model_1/dense_features_1/WDAY_N/Shape:output:0<model_1/dense_features_1/WDAY_N/strided_slice/stack:output:0>model_1/dense_features_1/WDAY_N/strided_slice/stack_1:output:0>model_1/dense_features_1/WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2/
-model_1/dense_features_1/WDAY_N/strided_slice?
/model_1/dense_features_1/WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :21
/model_1/dense_features_1/WDAY_N/Reshape/shape/1?
-model_1/dense_features_1/WDAY_N/Reshape/shapePack6model_1/dense_features_1/WDAY_N/strided_slice:output:08model_1/dense_features_1/WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2/
-model_1/dense_features_1/WDAY_N/Reshape/shape?
'model_1/dense_features_1/WDAY_N/ReshapeReshape+model_1/dense_features_1/WDAY_N/truediv:z:06model_1/dense_features_1/WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2)
'model_1/dense_features_1/WDAY_N/Reshape?
,model_1/dense_features_1/WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,model_1/dense_features_1/WEEK/ExpandDims/dim?
(model_1/dense_features_1/WEEK/ExpandDims
ExpandDimsweek5model_1/dense_features_1/WEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2*
(model_1/dense_features_1/WEEK/ExpandDims?
$model_1/dense_features_1/WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2&
$model_1/dense_features_1/WEEK/Cast/x?
!model_1/dense_features_1/WEEK/SubSub1model_1/dense_features_1/WEEK/ExpandDims:output:0-model_1/dense_features_1/WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2#
!model_1/dense_features_1/WEEK/Sub?
&model_1/dense_features_1/WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2(
&model_1/dense_features_1/WEEK/Cast_1/x?
%model_1/dense_features_1/WEEK/truedivRealDiv%model_1/dense_features_1/WEEK/Sub:z:0/model_1/dense_features_1/WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2'
%model_1/dense_features_1/WEEK/truediv?
#model_1/dense_features_1/WEEK/ShapeShape)model_1/dense_features_1/WEEK/truediv:z:0*
T0*
_output_shapes
:2%
#model_1/dense_features_1/WEEK/Shape?
1model_1/dense_features_1/WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 23
1model_1/dense_features_1/WEEK/strided_slice/stack?
3model_1/dense_features_1/WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:25
3model_1/dense_features_1/WEEK/strided_slice/stack_1?
3model_1/dense_features_1/WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:25
3model_1/dense_features_1/WEEK/strided_slice/stack_2?
+model_1/dense_features_1/WEEK/strided_sliceStridedSlice,model_1/dense_features_1/WEEK/Shape:output:0:model_1/dense_features_1/WEEK/strided_slice/stack:output:0<model_1/dense_features_1/WEEK/strided_slice/stack_1:output:0<model_1/dense_features_1/WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2-
+model_1/dense_features_1/WEEK/strided_slice?
-model_1/dense_features_1/WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2/
-model_1/dense_features_1/WEEK/Reshape/shape/1?
+model_1/dense_features_1/WEEK/Reshape/shapePack4model_1/dense_features_1/WEEK/strided_slice:output:06model_1/dense_features_1/WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2-
+model_1/dense_features_1/WEEK/Reshape/shape?
%model_1/dense_features_1/WEEK/ReshapeReshape)model_1/dense_features_1/WEEK/truediv:z:04model_1/dense_features_1/WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2'
%model_1/dense_features_1/WEEK/Reshape?
,model_1/dense_features_1/YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,model_1/dense_features_1/YEAR/ExpandDims/dim?
(model_1/dense_features_1/YEAR/ExpandDims
ExpandDimsyear5model_1/dense_features_1/YEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2*
(model_1/dense_features_1/YEAR/ExpandDims?
$model_1/dense_features_1/YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2&
$model_1/dense_features_1/YEAR/Cast/x?
!model_1/dense_features_1/YEAR/SubSub1model_1/dense_features_1/YEAR/ExpandDims:output:0-model_1/dense_features_1/YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2#
!model_1/dense_features_1/YEAR/Sub?
&model_1/dense_features_1/YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2(
&model_1/dense_features_1/YEAR/Cast_1/x?
%model_1/dense_features_1/YEAR/truedivRealDiv%model_1/dense_features_1/YEAR/Sub:z:0/model_1/dense_features_1/YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2'
%model_1/dense_features_1/YEAR/truediv?
#model_1/dense_features_1/YEAR/ShapeShape)model_1/dense_features_1/YEAR/truediv:z:0*
T0*
_output_shapes
:2%
#model_1/dense_features_1/YEAR/Shape?
1model_1/dense_features_1/YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 23
1model_1/dense_features_1/YEAR/strided_slice/stack?
3model_1/dense_features_1/YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:25
3model_1/dense_features_1/YEAR/strided_slice/stack_1?
3model_1/dense_features_1/YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:25
3model_1/dense_features_1/YEAR/strided_slice/stack_2?
+model_1/dense_features_1/YEAR/strided_sliceStridedSlice,model_1/dense_features_1/YEAR/Shape:output:0:model_1/dense_features_1/YEAR/strided_slice/stack:output:0<model_1/dense_features_1/YEAR/strided_slice/stack_1:output:0<model_1/dense_features_1/YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2-
+model_1/dense_features_1/YEAR/strided_slice?
-model_1/dense_features_1/YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2/
-model_1/dense_features_1/YEAR/Reshape/shape/1?
+model_1/dense_features_1/YEAR/Reshape/shapePack4model_1/dense_features_1/YEAR/strided_slice:output:06model_1/dense_features_1/YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2-
+model_1/dense_features_1/YEAR/Reshape/shape?
%model_1/dense_features_1/YEAR/ReshapeReshape)model_1/dense_features_1/YEAR/truediv:z:04model_1/dense_features_1/YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2'
%model_1/dense_features_1/YEAR/Reshape?
$model_1/dense_features_1/concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2&
$model_1/dense_features_1/concat/axis?
model_1/dense_features_1/concatConcatV23model_1/dense_features_1/AVAILABLE/Reshape:output:01model_1/dense_features_1/GROUP_N/Reshape:output:0<model_1/dense_features_1/TOTAL_APPLICATIONS/Reshape:output:00model_1/dense_features_1/WDAY_N/Reshape:output:0.model_1/dense_features_1/WEEK/Reshape:output:0.model_1/dense_features_1/YEAR/Reshape:output:0-model_1/dense_features_1/concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2!
model_1/dense_features_1/concat?
%model_1/dense_5/MatMul/ReadVariableOpReadVariableOp.model_1_dense_5_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02'
%model_1/dense_5/MatMul/ReadVariableOp?
model_1/dense_5/MatMulMatMul(model_1/dense_features_1/concat:output:0-model_1/dense_5/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_5/MatMul?
&model_1/dense_5/BiasAdd/ReadVariableOpReadVariableOp/model_1_dense_5_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02(
&model_1/dense_5/BiasAdd/ReadVariableOp?
model_1/dense_5/BiasAddBiasAdd model_1/dense_5/MatMul:product:0.model_1/dense_5/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_5/BiasAdd?
model_1/dense_5/ReluRelu model_1/dense_5/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_5/Relu?
%model_1/dense_4/MatMul/ReadVariableOpReadVariableOp.model_1_dense_4_matmul_readvariableop_resource*
_output_shapes

:@@*
dtype02'
%model_1/dense_4/MatMul/ReadVariableOp?
model_1/dense_4/MatMulMatMul"model_1/dense_5/Relu:activations:0-model_1/dense_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_4/MatMul?
&model_1/dense_4/BiasAdd/ReadVariableOpReadVariableOp/model_1_dense_4_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02(
&model_1/dense_4/BiasAdd/ReadVariableOp?
model_1/dense_4/BiasAddBiasAdd model_1/dense_4/MatMul:product:0.model_1/dense_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_4/BiasAdd?
model_1/dense_4/ReluRelu model_1/dense_4/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
model_1/dense_4/Relu?
%model_1/dense_3/MatMul/ReadVariableOpReadVariableOp.model_1_dense_3_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02'
%model_1/dense_3/MatMul/ReadVariableOp?
model_1/dense_3/MatMulMatMul"model_1/dense_4/Relu:activations:0-model_1/dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
model_1/dense_3/MatMul?
&model_1/dense_3/BiasAdd/ReadVariableOpReadVariableOp/model_1_dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02(
&model_1/dense_3/BiasAdd/ReadVariableOp?
model_1/dense_3/BiasAddBiasAdd model_1/dense_3/MatMul:product:0.model_1/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
model_1/dense_3/BiasAdd?
IdentityIdentity model_1/dense_3/BiasAdd:output:0'^model_1/dense_3/BiasAdd/ReadVariableOp&^model_1/dense_3/MatMul/ReadVariableOp'^model_1/dense_4/BiasAdd/ReadVariableOp&^model_1/dense_4/MatMul/ReadVariableOp'^model_1/dense_5/BiasAdd/ReadVariableOp&^model_1/dense_5/MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2P
&model_1/dense_3/BiasAdd/ReadVariableOp&model_1/dense_3/BiasAdd/ReadVariableOp2N
%model_1/dense_3/MatMul/ReadVariableOp%model_1/dense_3/MatMul/ReadVariableOp2P
&model_1/dense_4/BiasAdd/ReadVariableOp&model_1/dense_4/BiasAdd/ReadVariableOp2N
%model_1/dense_4/MatMul/ReadVariableOp%model_1/dense_4/MatMul/ReadVariableOp2P
&model_1/dense_5/BiasAdd/ReadVariableOp&model_1/dense_5/BiasAdd/ReadVariableOp2N
%model_1/dense_5/MatMul/ReadVariableOp%model_1/dense_5/MatMul/ReadVariableOp:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?
?
#__inference_signature_wrapper_23112
	available
group_n
total_applications

wday_n
week
year
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall	availablegroup_ntotal_applicationswday_nweekyearunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

	
*-
config_proto

CPU

GPU 2J 8? *)
f$R"
 __inference__wrapped_model_226572
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::22
StatefulPartitionedCallStatefulPartitionedCall:N J
#
_output_shapes
:?????????
#
_user_specified_name	AVAILABLE:LH
#
_output_shapes
:?????????
!
_user_specified_name	GROUP_N:WS
#
_output_shapes
:?????????
,
_user_specified_nameTOTAL_APPLICATIONS:KG
#
_output_shapes
:?????????
 
_user_specified_nameWDAY_N:IE
#
_output_shapes
:?????????

_user_specified_nameWEEK:IE
#
_output_shapes
:?????????

_user_specified_nameYEAR
?
?
0__inference_dense_features_1_layer_call_fn_23586
features_available
features_group_n
features_total_applications
features_wday_n
features_week
features_year
identity?
PartitionedCallPartitionedCallfeatures_availablefeatures_group_nfeatures_total_applicationsfeatures_wday_nfeatures_weekfeatures_year*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_227612
PartitionedCalll
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:W S
#
_output_shapes
:?????????
,
_user_specified_namefeatures/AVAILABLE:UQ
#
_output_shapes
:?????????
*
_user_specified_namefeatures/GROUP_N:`\
#
_output_shapes
:?????????
5
_user_specified_namefeatures/TOTAL_APPLICATIONS:TP
#
_output_shapes
:?????????
)
_user_specified_namefeatures/WDAY_N:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/WEEK:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/YEAR
??
?
B__inference_model_1_layer_call_and_return_conditional_losses_23227
inputs_available
inputs_group_n
inputs_total_applications
inputs_wday_n
inputs_week
inputs_year*
&dense_5_matmul_readvariableop_resource+
'dense_5_biasadd_readvariableop_resource*
&dense_4_matmul_readvariableop_resource+
'dense_4_biasadd_readvariableop_resource*
&dense_3_matmul_readvariableop_resource+
'dense_3_biasadd_readvariableop_resource
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOp?dense_4/BiasAdd/ReadVariableOp?dense_4/MatMul/ReadVariableOp?dense_5/BiasAdd/ReadVariableOp?dense_5/MatMul/ReadVariableOp?
)dense_features_1/AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2+
)dense_features_1/AVAILABLE/ExpandDims/dim?
%dense_features_1/AVAILABLE/ExpandDims
ExpandDimsinputs_available2dense_features_1/AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2'
%dense_features_1/AVAILABLE/ExpandDims?
!dense_features_1/AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2#
!dense_features_1/AVAILABLE/Cast/x?
dense_features_1/AVAILABLE/SubSub.dense_features_1/AVAILABLE/ExpandDims:output:0*dense_features_1/AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2 
dense_features_1/AVAILABLE/Sub?
#dense_features_1/AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2%
#dense_features_1/AVAILABLE/Cast_1/x?
"dense_features_1/AVAILABLE/truedivRealDiv"dense_features_1/AVAILABLE/Sub:z:0,dense_features_1/AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/AVAILABLE/truediv?
 dense_features_1/AVAILABLE/ShapeShape&dense_features_1/AVAILABLE/truediv:z:0*
T0*
_output_shapes
:2"
 dense_features_1/AVAILABLE/Shape?
.dense_features_1/AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 20
.dense_features_1/AVAILABLE/strided_slice/stack?
0dense_features_1/AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:22
0dense_features_1/AVAILABLE/strided_slice/stack_1?
0dense_features_1/AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:22
0dense_features_1/AVAILABLE/strided_slice/stack_2?
(dense_features_1/AVAILABLE/strided_sliceStridedSlice)dense_features_1/AVAILABLE/Shape:output:07dense_features_1/AVAILABLE/strided_slice/stack:output:09dense_features_1/AVAILABLE/strided_slice/stack_1:output:09dense_features_1/AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2*
(dense_features_1/AVAILABLE/strided_slice?
*dense_features_1/AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2,
*dense_features_1/AVAILABLE/Reshape/shape/1?
(dense_features_1/AVAILABLE/Reshape/shapePack1dense_features_1/AVAILABLE/strided_slice:output:03dense_features_1/AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2*
(dense_features_1/AVAILABLE/Reshape/shape?
"dense_features_1/AVAILABLE/ReshapeReshape&dense_features_1/AVAILABLE/truediv:z:01dense_features_1/AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/AVAILABLE/Reshape?
'dense_features_1/GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2)
'dense_features_1/GROUP_N/ExpandDims/dim?
#dense_features_1/GROUP_N/ExpandDims
ExpandDimsinputs_group_n0dense_features_1/GROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2%
#dense_features_1/GROUP_N/ExpandDims?
dense_features_1/GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2!
dense_features_1/GROUP_N/Cast/x?
dense_features_1/GROUP_N/SubSub,dense_features_1/GROUP_N/ExpandDims:output:0(dense_features_1/GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/GROUP_N/Sub?
!dense_features_1/GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2#
!dense_features_1/GROUP_N/Cast_1/x?
 dense_features_1/GROUP_N/truedivRealDiv dense_features_1/GROUP_N/Sub:z:0*dense_features_1/GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/GROUP_N/truediv?
dense_features_1/GROUP_N/ShapeShape$dense_features_1/GROUP_N/truediv:z:0*
T0*
_output_shapes
:2 
dense_features_1/GROUP_N/Shape?
,dense_features_1/GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2.
,dense_features_1/GROUP_N/strided_slice/stack?
.dense_features_1/GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:20
.dense_features_1/GROUP_N/strided_slice/stack_1?
.dense_features_1/GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:20
.dense_features_1/GROUP_N/strided_slice/stack_2?
&dense_features_1/GROUP_N/strided_sliceStridedSlice'dense_features_1/GROUP_N/Shape:output:05dense_features_1/GROUP_N/strided_slice/stack:output:07dense_features_1/GROUP_N/strided_slice/stack_1:output:07dense_features_1/GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2(
&dense_features_1/GROUP_N/strided_slice?
(dense_features_1/GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2*
(dense_features_1/GROUP_N/Reshape/shape/1?
&dense_features_1/GROUP_N/Reshape/shapePack/dense_features_1/GROUP_N/strided_slice:output:01dense_features_1/GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2(
&dense_features_1/GROUP_N/Reshape/shape?
 dense_features_1/GROUP_N/ReshapeReshape$dense_features_1/GROUP_N/truediv:z:0/dense_features_1/GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/GROUP_N/Reshape?
2dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????24
2dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim?
.dense_features_1/TOTAL_APPLICATIONS/ExpandDims
ExpandDimsinputs_total_applications;dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????20
.dense_features_1/TOTAL_APPLICATIONS/ExpandDims?
*dense_features_1/TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2,
*dense_features_1/TOTAL_APPLICATIONS/Cast/x?
'dense_features_1/TOTAL_APPLICATIONS/SubSub7dense_features_1/TOTAL_APPLICATIONS/ExpandDims:output:03dense_features_1/TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2)
'dense_features_1/TOTAL_APPLICATIONS/Sub?
,dense_features_1/TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2.
,dense_features_1/TOTAL_APPLICATIONS/Cast_1/x?
+dense_features_1/TOTAL_APPLICATIONS/truedivRealDiv+dense_features_1/TOTAL_APPLICATIONS/Sub:z:05dense_features_1/TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2-
+dense_features_1/TOTAL_APPLICATIONS/truediv?
)dense_features_1/TOTAL_APPLICATIONS/ShapeShape/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2+
)dense_features_1/TOTAL_APPLICATIONS/Shape?
7dense_features_1/TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 29
7dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack?
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2;
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1?
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2;
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2?
1dense_features_1/TOTAL_APPLICATIONS/strided_sliceStridedSlice2dense_features_1/TOTAL_APPLICATIONS/Shape:output:0@dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack:output:0Bdense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1:output:0Bdense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask23
1dense_features_1/TOTAL_APPLICATIONS/strided_slice?
3dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :25
3dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1?
1dense_features_1/TOTAL_APPLICATIONS/Reshape/shapePack:dense_features_1/TOTAL_APPLICATIONS/strided_slice:output:0<dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:23
1dense_features_1/TOTAL_APPLICATIONS/Reshape/shape?
+dense_features_1/TOTAL_APPLICATIONS/ReshapeReshape/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0:dense_features_1/TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2-
+dense_features_1/TOTAL_APPLICATIONS/Reshape?
&dense_features_1/WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2(
&dense_features_1/WDAY_N/ExpandDims/dim?
"dense_features_1/WDAY_N/ExpandDims
ExpandDimsinputs_wday_n/dense_features_1/WDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/WDAY_N/ExpandDims?
dense_features_1/WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2 
dense_features_1/WDAY_N/Cast/x?
dense_features_1/WDAY_N/SubSub+dense_features_1/WDAY_N/ExpandDims:output:0'dense_features_1/WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WDAY_N/Sub?
 dense_features_1/WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2"
 dense_features_1/WDAY_N/Cast_1/x?
dense_features_1/WDAY_N/truedivRealDivdense_features_1/WDAY_N/Sub:z:0)dense_features_1/WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2!
dense_features_1/WDAY_N/truediv?
dense_features_1/WDAY_N/ShapeShape#dense_features_1/WDAY_N/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/WDAY_N/Shape?
+dense_features_1/WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2-
+dense_features_1/WDAY_N/strided_slice/stack?
-dense_features_1/WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2/
-dense_features_1/WDAY_N/strided_slice/stack_1?
-dense_features_1/WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-dense_features_1/WDAY_N/strided_slice/stack_2?
%dense_features_1/WDAY_N/strided_sliceStridedSlice&dense_features_1/WDAY_N/Shape:output:04dense_features_1/WDAY_N/strided_slice/stack:output:06dense_features_1/WDAY_N/strided_slice/stack_1:output:06dense_features_1/WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2'
%dense_features_1/WDAY_N/strided_slice?
'dense_features_1/WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2)
'dense_features_1/WDAY_N/Reshape/shape/1?
%dense_features_1/WDAY_N/Reshape/shapePack.dense_features_1/WDAY_N/strided_slice:output:00dense_features_1/WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2'
%dense_features_1/WDAY_N/Reshape/shape?
dense_features_1/WDAY_N/ReshapeReshape#dense_features_1/WDAY_N/truediv:z:0.dense_features_1/WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2!
dense_features_1/WDAY_N/Reshape?
$dense_features_1/WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2&
$dense_features_1/WEEK/ExpandDims/dim?
 dense_features_1/WEEK/ExpandDims
ExpandDimsinputs_week-dense_features_1/WEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/WEEK/ExpandDims?
dense_features_1/WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
dense_features_1/WEEK/Cast/x?
dense_features_1/WEEK/SubSub)dense_features_1/WEEK/ExpandDims:output:0%dense_features_1/WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/Sub?
dense_features_1/WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2 
dense_features_1/WEEK/Cast_1/x?
dense_features_1/WEEK/truedivRealDivdense_features_1/WEEK/Sub:z:0'dense_features_1/WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/truediv?
dense_features_1/WEEK/ShapeShape!dense_features_1/WEEK/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/WEEK/Shape?
)dense_features_1/WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)dense_features_1/WEEK/strided_slice/stack?
+dense_features_1/WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/WEEK/strided_slice/stack_1?
+dense_features_1/WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/WEEK/strided_slice/stack_2?
#dense_features_1/WEEK/strided_sliceStridedSlice$dense_features_1/WEEK/Shape:output:02dense_features_1/WEEK/strided_slice/stack:output:04dense_features_1/WEEK/strided_slice/stack_1:output:04dense_features_1/WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#dense_features_1/WEEK/strided_slice?
%dense_features_1/WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%dense_features_1/WEEK/Reshape/shape/1?
#dense_features_1/WEEK/Reshape/shapePack,dense_features_1/WEEK/strided_slice:output:0.dense_features_1/WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2%
#dense_features_1/WEEK/Reshape/shape?
dense_features_1/WEEK/ReshapeReshape!dense_features_1/WEEK/truediv:z:0,dense_features_1/WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/Reshape?
$dense_features_1/YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2&
$dense_features_1/YEAR/ExpandDims/dim?
 dense_features_1/YEAR/ExpandDims
ExpandDimsinputs_year-dense_features_1/YEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/YEAR/ExpandDims?
dense_features_1/YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
dense_features_1/YEAR/Cast/x?
dense_features_1/YEAR/SubSub)dense_features_1/YEAR/ExpandDims:output:0%dense_features_1/YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/Sub?
dense_features_1/YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2 
dense_features_1/YEAR/Cast_1/x?
dense_features_1/YEAR/truedivRealDivdense_features_1/YEAR/Sub:z:0'dense_features_1/YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/truediv?
dense_features_1/YEAR/ShapeShape!dense_features_1/YEAR/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/YEAR/Shape?
)dense_features_1/YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)dense_features_1/YEAR/strided_slice/stack?
+dense_features_1/YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/YEAR/strided_slice/stack_1?
+dense_features_1/YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/YEAR/strided_slice/stack_2?
#dense_features_1/YEAR/strided_sliceStridedSlice$dense_features_1/YEAR/Shape:output:02dense_features_1/YEAR/strided_slice/stack:output:04dense_features_1/YEAR/strided_slice/stack_1:output:04dense_features_1/YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#dense_features_1/YEAR/strided_slice?
%dense_features_1/YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%dense_features_1/YEAR/Reshape/shape/1?
#dense_features_1/YEAR/Reshape/shapePack,dense_features_1/YEAR/strided_slice:output:0.dense_features_1/YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2%
#dense_features_1/YEAR/Reshape/shape?
dense_features_1/YEAR/ReshapeReshape!dense_features_1/YEAR/truediv:z:0,dense_features_1/YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/Reshape?
dense_features_1/concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
dense_features_1/concat/axis?
dense_features_1/concatConcatV2+dense_features_1/AVAILABLE/Reshape:output:0)dense_features_1/GROUP_N/Reshape:output:04dense_features_1/TOTAL_APPLICATIONS/Reshape:output:0(dense_features_1/WDAY_N/Reshape:output:0&dense_features_1/WEEK/Reshape:output:0&dense_features_1/YEAR/Reshape:output:0%dense_features_1/concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
dense_features_1/concat?
dense_5/MatMul/ReadVariableOpReadVariableOp&dense_5_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02
dense_5/MatMul/ReadVariableOp?
dense_5/MatMulMatMul dense_features_1/concat:output:0%dense_5/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_5/MatMul?
dense_5/BiasAdd/ReadVariableOpReadVariableOp'dense_5_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02 
dense_5/BiasAdd/ReadVariableOp?
dense_5/BiasAddBiasAdddense_5/MatMul:product:0&dense_5/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_5/BiasAddp
dense_5/ReluReludense_5/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_5/Relu?
dense_4/MatMul/ReadVariableOpReadVariableOp&dense_4_matmul_readvariableop_resource*
_output_shapes

:@@*
dtype02
dense_4/MatMul/ReadVariableOp?
dense_4/MatMulMatMuldense_5/Relu:activations:0%dense_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_4/MatMul?
dense_4/BiasAdd/ReadVariableOpReadVariableOp'dense_4_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02 
dense_4/BiasAdd/ReadVariableOp?
dense_4/BiasAddBiasAdddense_4/MatMul:product:0&dense_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_4/BiasAddp
dense_4/ReluReludense_4/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_4/Relu?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMuldense_4/Relu:activations:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/BiasAdd?
IdentityIdentitydense_3/BiasAdd:output:0^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^dense_4/BiasAdd/ReadVariableOp^dense_4/MatMul/ReadVariableOp^dense_5/BiasAdd/ReadVariableOp^dense_5/MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp2@
dense_4/BiasAdd/ReadVariableOpdense_4/BiasAdd/ReadVariableOp2>
dense_4/MatMul/ReadVariableOpdense_4/MatMul/ReadVariableOp2@
dense_5/BiasAdd/ReadVariableOpdense_5/BiasAdd/ReadVariableOp2>
dense_5/MatMul/ReadVariableOpdense_5/MatMul/ReadVariableOp:U Q
#
_output_shapes
:?????????
*
_user_specified_nameinputs/AVAILABLE:SO
#
_output_shapes
:?????????
(
_user_specified_nameinputs/GROUP_N:^Z
#
_output_shapes
:?????????
3
_user_specified_nameinputs/TOTAL_APPLICATIONS:RN
#
_output_shapes
:?????????
'
_user_specified_nameinputs/WDAY_N:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/WEEK:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/YEAR
??
?
B__inference_model_1_layer_call_and_return_conditional_losses_23342
inputs_available
inputs_group_n
inputs_total_applications
inputs_wday_n
inputs_week
inputs_year*
&dense_5_matmul_readvariableop_resource+
'dense_5_biasadd_readvariableop_resource*
&dense_4_matmul_readvariableop_resource+
'dense_4_biasadd_readvariableop_resource*
&dense_3_matmul_readvariableop_resource+
'dense_3_biasadd_readvariableop_resource
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOp?dense_4/BiasAdd/ReadVariableOp?dense_4/MatMul/ReadVariableOp?dense_5/BiasAdd/ReadVariableOp?dense_5/MatMul/ReadVariableOp?
)dense_features_1/AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2+
)dense_features_1/AVAILABLE/ExpandDims/dim?
%dense_features_1/AVAILABLE/ExpandDims
ExpandDimsinputs_available2dense_features_1/AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2'
%dense_features_1/AVAILABLE/ExpandDims?
!dense_features_1/AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2#
!dense_features_1/AVAILABLE/Cast/x?
dense_features_1/AVAILABLE/SubSub.dense_features_1/AVAILABLE/ExpandDims:output:0*dense_features_1/AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2 
dense_features_1/AVAILABLE/Sub?
#dense_features_1/AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2%
#dense_features_1/AVAILABLE/Cast_1/x?
"dense_features_1/AVAILABLE/truedivRealDiv"dense_features_1/AVAILABLE/Sub:z:0,dense_features_1/AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/AVAILABLE/truediv?
 dense_features_1/AVAILABLE/ShapeShape&dense_features_1/AVAILABLE/truediv:z:0*
T0*
_output_shapes
:2"
 dense_features_1/AVAILABLE/Shape?
.dense_features_1/AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 20
.dense_features_1/AVAILABLE/strided_slice/stack?
0dense_features_1/AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:22
0dense_features_1/AVAILABLE/strided_slice/stack_1?
0dense_features_1/AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:22
0dense_features_1/AVAILABLE/strided_slice/stack_2?
(dense_features_1/AVAILABLE/strided_sliceStridedSlice)dense_features_1/AVAILABLE/Shape:output:07dense_features_1/AVAILABLE/strided_slice/stack:output:09dense_features_1/AVAILABLE/strided_slice/stack_1:output:09dense_features_1/AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2*
(dense_features_1/AVAILABLE/strided_slice?
*dense_features_1/AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2,
*dense_features_1/AVAILABLE/Reshape/shape/1?
(dense_features_1/AVAILABLE/Reshape/shapePack1dense_features_1/AVAILABLE/strided_slice:output:03dense_features_1/AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2*
(dense_features_1/AVAILABLE/Reshape/shape?
"dense_features_1/AVAILABLE/ReshapeReshape&dense_features_1/AVAILABLE/truediv:z:01dense_features_1/AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/AVAILABLE/Reshape?
'dense_features_1/GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2)
'dense_features_1/GROUP_N/ExpandDims/dim?
#dense_features_1/GROUP_N/ExpandDims
ExpandDimsinputs_group_n0dense_features_1/GROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2%
#dense_features_1/GROUP_N/ExpandDims?
dense_features_1/GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2!
dense_features_1/GROUP_N/Cast/x?
dense_features_1/GROUP_N/SubSub,dense_features_1/GROUP_N/ExpandDims:output:0(dense_features_1/GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/GROUP_N/Sub?
!dense_features_1/GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2#
!dense_features_1/GROUP_N/Cast_1/x?
 dense_features_1/GROUP_N/truedivRealDiv dense_features_1/GROUP_N/Sub:z:0*dense_features_1/GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/GROUP_N/truediv?
dense_features_1/GROUP_N/ShapeShape$dense_features_1/GROUP_N/truediv:z:0*
T0*
_output_shapes
:2 
dense_features_1/GROUP_N/Shape?
,dense_features_1/GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2.
,dense_features_1/GROUP_N/strided_slice/stack?
.dense_features_1/GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:20
.dense_features_1/GROUP_N/strided_slice/stack_1?
.dense_features_1/GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:20
.dense_features_1/GROUP_N/strided_slice/stack_2?
&dense_features_1/GROUP_N/strided_sliceStridedSlice'dense_features_1/GROUP_N/Shape:output:05dense_features_1/GROUP_N/strided_slice/stack:output:07dense_features_1/GROUP_N/strided_slice/stack_1:output:07dense_features_1/GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2(
&dense_features_1/GROUP_N/strided_slice?
(dense_features_1/GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2*
(dense_features_1/GROUP_N/Reshape/shape/1?
&dense_features_1/GROUP_N/Reshape/shapePack/dense_features_1/GROUP_N/strided_slice:output:01dense_features_1/GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2(
&dense_features_1/GROUP_N/Reshape/shape?
 dense_features_1/GROUP_N/ReshapeReshape$dense_features_1/GROUP_N/truediv:z:0/dense_features_1/GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/GROUP_N/Reshape?
2dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????24
2dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim?
.dense_features_1/TOTAL_APPLICATIONS/ExpandDims
ExpandDimsinputs_total_applications;dense_features_1/TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????20
.dense_features_1/TOTAL_APPLICATIONS/ExpandDims?
*dense_features_1/TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2,
*dense_features_1/TOTAL_APPLICATIONS/Cast/x?
'dense_features_1/TOTAL_APPLICATIONS/SubSub7dense_features_1/TOTAL_APPLICATIONS/ExpandDims:output:03dense_features_1/TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2)
'dense_features_1/TOTAL_APPLICATIONS/Sub?
,dense_features_1/TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2.
,dense_features_1/TOTAL_APPLICATIONS/Cast_1/x?
+dense_features_1/TOTAL_APPLICATIONS/truedivRealDiv+dense_features_1/TOTAL_APPLICATIONS/Sub:z:05dense_features_1/TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2-
+dense_features_1/TOTAL_APPLICATIONS/truediv?
)dense_features_1/TOTAL_APPLICATIONS/ShapeShape/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2+
)dense_features_1/TOTAL_APPLICATIONS/Shape?
7dense_features_1/TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 29
7dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack?
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2;
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1?
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2;
9dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2?
1dense_features_1/TOTAL_APPLICATIONS/strided_sliceStridedSlice2dense_features_1/TOTAL_APPLICATIONS/Shape:output:0@dense_features_1/TOTAL_APPLICATIONS/strided_slice/stack:output:0Bdense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_1:output:0Bdense_features_1/TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask23
1dense_features_1/TOTAL_APPLICATIONS/strided_slice?
3dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :25
3dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1?
1dense_features_1/TOTAL_APPLICATIONS/Reshape/shapePack:dense_features_1/TOTAL_APPLICATIONS/strided_slice:output:0<dense_features_1/TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:23
1dense_features_1/TOTAL_APPLICATIONS/Reshape/shape?
+dense_features_1/TOTAL_APPLICATIONS/ReshapeReshape/dense_features_1/TOTAL_APPLICATIONS/truediv:z:0:dense_features_1/TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2-
+dense_features_1/TOTAL_APPLICATIONS/Reshape?
&dense_features_1/WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2(
&dense_features_1/WDAY_N/ExpandDims/dim?
"dense_features_1/WDAY_N/ExpandDims
ExpandDimsinputs_wday_n/dense_features_1/WDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2$
"dense_features_1/WDAY_N/ExpandDims?
dense_features_1/WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2 
dense_features_1/WDAY_N/Cast/x?
dense_features_1/WDAY_N/SubSub+dense_features_1/WDAY_N/ExpandDims:output:0'dense_features_1/WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WDAY_N/Sub?
 dense_features_1/WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2"
 dense_features_1/WDAY_N/Cast_1/x?
dense_features_1/WDAY_N/truedivRealDivdense_features_1/WDAY_N/Sub:z:0)dense_features_1/WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2!
dense_features_1/WDAY_N/truediv?
dense_features_1/WDAY_N/ShapeShape#dense_features_1/WDAY_N/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/WDAY_N/Shape?
+dense_features_1/WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2-
+dense_features_1/WDAY_N/strided_slice/stack?
-dense_features_1/WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2/
-dense_features_1/WDAY_N/strided_slice/stack_1?
-dense_features_1/WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-dense_features_1/WDAY_N/strided_slice/stack_2?
%dense_features_1/WDAY_N/strided_sliceStridedSlice&dense_features_1/WDAY_N/Shape:output:04dense_features_1/WDAY_N/strided_slice/stack:output:06dense_features_1/WDAY_N/strided_slice/stack_1:output:06dense_features_1/WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2'
%dense_features_1/WDAY_N/strided_slice?
'dense_features_1/WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2)
'dense_features_1/WDAY_N/Reshape/shape/1?
%dense_features_1/WDAY_N/Reshape/shapePack.dense_features_1/WDAY_N/strided_slice:output:00dense_features_1/WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2'
%dense_features_1/WDAY_N/Reshape/shape?
dense_features_1/WDAY_N/ReshapeReshape#dense_features_1/WDAY_N/truediv:z:0.dense_features_1/WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2!
dense_features_1/WDAY_N/Reshape?
$dense_features_1/WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2&
$dense_features_1/WEEK/ExpandDims/dim?
 dense_features_1/WEEK/ExpandDims
ExpandDimsinputs_week-dense_features_1/WEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/WEEK/ExpandDims?
dense_features_1/WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
dense_features_1/WEEK/Cast/x?
dense_features_1/WEEK/SubSub)dense_features_1/WEEK/ExpandDims:output:0%dense_features_1/WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/Sub?
dense_features_1/WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2 
dense_features_1/WEEK/Cast_1/x?
dense_features_1/WEEK/truedivRealDivdense_features_1/WEEK/Sub:z:0'dense_features_1/WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/truediv?
dense_features_1/WEEK/ShapeShape!dense_features_1/WEEK/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/WEEK/Shape?
)dense_features_1/WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)dense_features_1/WEEK/strided_slice/stack?
+dense_features_1/WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/WEEK/strided_slice/stack_1?
+dense_features_1/WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/WEEK/strided_slice/stack_2?
#dense_features_1/WEEK/strided_sliceStridedSlice$dense_features_1/WEEK/Shape:output:02dense_features_1/WEEK/strided_slice/stack:output:04dense_features_1/WEEK/strided_slice/stack_1:output:04dense_features_1/WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#dense_features_1/WEEK/strided_slice?
%dense_features_1/WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%dense_features_1/WEEK/Reshape/shape/1?
#dense_features_1/WEEK/Reshape/shapePack,dense_features_1/WEEK/strided_slice:output:0.dense_features_1/WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2%
#dense_features_1/WEEK/Reshape/shape?
dense_features_1/WEEK/ReshapeReshape!dense_features_1/WEEK/truediv:z:0,dense_features_1/WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/WEEK/Reshape?
$dense_features_1/YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2&
$dense_features_1/YEAR/ExpandDims/dim?
 dense_features_1/YEAR/ExpandDims
ExpandDimsinputs_year-dense_features_1/YEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2"
 dense_features_1/YEAR/ExpandDims?
dense_features_1/YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
dense_features_1/YEAR/Cast/x?
dense_features_1/YEAR/SubSub)dense_features_1/YEAR/ExpandDims:output:0%dense_features_1/YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/Sub?
dense_features_1/YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2 
dense_features_1/YEAR/Cast_1/x?
dense_features_1/YEAR/truedivRealDivdense_features_1/YEAR/Sub:z:0'dense_features_1/YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/truediv?
dense_features_1/YEAR/ShapeShape!dense_features_1/YEAR/truediv:z:0*
T0*
_output_shapes
:2
dense_features_1/YEAR/Shape?
)dense_features_1/YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)dense_features_1/YEAR/strided_slice/stack?
+dense_features_1/YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/YEAR/strided_slice/stack_1?
+dense_features_1/YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+dense_features_1/YEAR/strided_slice/stack_2?
#dense_features_1/YEAR/strided_sliceStridedSlice$dense_features_1/YEAR/Shape:output:02dense_features_1/YEAR/strided_slice/stack:output:04dense_features_1/YEAR/strided_slice/stack_1:output:04dense_features_1/YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#dense_features_1/YEAR/strided_slice?
%dense_features_1/YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%dense_features_1/YEAR/Reshape/shape/1?
#dense_features_1/YEAR/Reshape/shapePack,dense_features_1/YEAR/strided_slice:output:0.dense_features_1/YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2%
#dense_features_1/YEAR/Reshape/shape?
dense_features_1/YEAR/ReshapeReshape!dense_features_1/YEAR/truediv:z:0,dense_features_1/YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
dense_features_1/YEAR/Reshape?
dense_features_1/concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
dense_features_1/concat/axis?
dense_features_1/concatConcatV2+dense_features_1/AVAILABLE/Reshape:output:0)dense_features_1/GROUP_N/Reshape:output:04dense_features_1/TOTAL_APPLICATIONS/Reshape:output:0(dense_features_1/WDAY_N/Reshape:output:0&dense_features_1/WEEK/Reshape:output:0&dense_features_1/YEAR/Reshape:output:0%dense_features_1/concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
dense_features_1/concat?
dense_5/MatMul/ReadVariableOpReadVariableOp&dense_5_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02
dense_5/MatMul/ReadVariableOp?
dense_5/MatMulMatMul dense_features_1/concat:output:0%dense_5/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_5/MatMul?
dense_5/BiasAdd/ReadVariableOpReadVariableOp'dense_5_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02 
dense_5/BiasAdd/ReadVariableOp?
dense_5/BiasAddBiasAdddense_5/MatMul:product:0&dense_5/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_5/BiasAddp
dense_5/ReluReludense_5/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_5/Relu?
dense_4/MatMul/ReadVariableOpReadVariableOp&dense_4_matmul_readvariableop_resource*
_output_shapes

:@@*
dtype02
dense_4/MatMul/ReadVariableOp?
dense_4/MatMulMatMuldense_5/Relu:activations:0%dense_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_4/MatMul?
dense_4/BiasAdd/ReadVariableOpReadVariableOp'dense_4_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02 
dense_4/BiasAdd/ReadVariableOp?
dense_4/BiasAddBiasAdddense_4/MatMul:product:0&dense_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_4/BiasAddp
dense_4/ReluReludense_4/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_4/Relu?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMuldense_4/Relu:activations:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/BiasAdd?
IdentityIdentitydense_3/BiasAdd:output:0^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^dense_4/BiasAdd/ReadVariableOp^dense_4/MatMul/ReadVariableOp^dense_5/BiasAdd/ReadVariableOp^dense_5/MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp2@
dense_4/BiasAdd/ReadVariableOpdense_4/BiasAdd/ReadVariableOp2>
dense_4/MatMul/ReadVariableOpdense_4/MatMul/ReadVariableOp2@
dense_5/BiasAdd/ReadVariableOpdense_5/BiasAdd/ReadVariableOp2>
dense_5/MatMul/ReadVariableOpdense_5/MatMul/ReadVariableOp:U Q
#
_output_shapes
:?????????
*
_user_specified_nameinputs/AVAILABLE:SO
#
_output_shapes
:?????????
(
_user_specified_nameinputs/GROUP_N:^Z
#
_output_shapes
:?????????
3
_user_specified_nameinputs/TOTAL_APPLICATIONS:RN
#
_output_shapes
:?????????
'
_user_specified_nameinputs/WDAY_N:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/WEEK:PL
#
_output_shapes
:?????????
%
_user_specified_nameinputs/YEAR
?j
?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23576
features_available
features_group_n
features_total_applications
features_wday_n
features_week
features_year
identity
AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
AVAILABLE/ExpandDims/dim?
AVAILABLE/ExpandDims
ExpandDimsfeatures_available!AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/ExpandDimsi
AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2
AVAILABLE/Cast/x?
AVAILABLE/SubSubAVAILABLE/ExpandDims:output:0AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Subm
AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2
AVAILABLE/Cast_1/x?
AVAILABLE/truedivRealDivAVAILABLE/Sub:z:0AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/truedivg
AVAILABLE/ShapeShapeAVAILABLE/truediv:z:0*
T0*
_output_shapes
:2
AVAILABLE/Shape?
AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
AVAILABLE/strided_slice/stack?
AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_1?
AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_2?
AVAILABLE/strided_sliceStridedSliceAVAILABLE/Shape:output:0&AVAILABLE/strided_slice/stack:output:0(AVAILABLE/strided_slice/stack_1:output:0(AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
AVAILABLE/strided_slicex
AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
AVAILABLE/Reshape/shape/1?
AVAILABLE/Reshape/shapePack AVAILABLE/strided_slice:output:0"AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
AVAILABLE/Reshape/shape?
AVAILABLE/ReshapeReshapeAVAILABLE/truediv:z:0 AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Reshape{
GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
GROUP_N/ExpandDims/dim?
GROUP_N/ExpandDims
ExpandDimsfeatures_group_nGROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/ExpandDimse
GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2
GROUP_N/Cast/x?
GROUP_N/SubSubGROUP_N/ExpandDims:output:0GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Subi
GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2
GROUP_N/Cast_1/x?
GROUP_N/truedivRealDivGROUP_N/Sub:z:0GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/truediva
GROUP_N/ShapeShapeGROUP_N/truediv:z:0*
T0*
_output_shapes
:2
GROUP_N/Shape?
GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
GROUP_N/strided_slice/stack?
GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_1?
GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_2?
GROUP_N/strided_sliceStridedSliceGROUP_N/Shape:output:0$GROUP_N/strided_slice/stack:output:0&GROUP_N/strided_slice/stack_1:output:0&GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
GROUP_N/strided_slicet
GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
GROUP_N/Reshape/shape/1?
GROUP_N/Reshape/shapePackGROUP_N/strided_slice:output:0 GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
GROUP_N/Reshape/shape?
GROUP_N/ReshapeReshapeGROUP_N/truediv:z:0GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Reshape?
!TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2#
!TOTAL_APPLICATIONS/ExpandDims/dim?
TOTAL_APPLICATIONS/ExpandDims
ExpandDimsfeatures_total_applications*TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/ExpandDims{
TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2
TOTAL_APPLICATIONS/Cast/x?
TOTAL_APPLICATIONS/SubSub&TOTAL_APPLICATIONS/ExpandDims:output:0"TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Sub
TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2
TOTAL_APPLICATIONS/Cast_1/x?
TOTAL_APPLICATIONS/truedivRealDivTOTAL_APPLICATIONS/Sub:z:0$TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/truediv?
TOTAL_APPLICATIONS/ShapeShapeTOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2
TOTAL_APPLICATIONS/Shape?
&TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&TOTAL_APPLICATIONS/strided_slice/stack?
(TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_1?
(TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_2?
 TOTAL_APPLICATIONS/strided_sliceStridedSlice!TOTAL_APPLICATIONS/Shape:output:0/TOTAL_APPLICATIONS/strided_slice/stack:output:01TOTAL_APPLICATIONS/strided_slice/stack_1:output:01TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 TOTAL_APPLICATIONS/strided_slice?
"TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2$
"TOTAL_APPLICATIONS/Reshape/shape/1?
 TOTAL_APPLICATIONS/Reshape/shapePack)TOTAL_APPLICATIONS/strided_slice:output:0+TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2"
 TOTAL_APPLICATIONS/Reshape/shape?
TOTAL_APPLICATIONS/ReshapeReshapeTOTAL_APPLICATIONS/truediv:z:0)TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Reshapey
WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WDAY_N/ExpandDims/dim?
WDAY_N/ExpandDims
ExpandDimsfeatures_wday_nWDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/ExpandDimsc
WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2
WDAY_N/Cast/x?

WDAY_N/SubSubWDAY_N/ExpandDims:output:0WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WDAY_N/Subg
WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2
WDAY_N/Cast_1/x?
WDAY_N/truedivRealDivWDAY_N/Sub:z:0WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/truediv^
WDAY_N/ShapeShapeWDAY_N/truediv:z:0*
T0*
_output_shapes
:2
WDAY_N/Shape?
WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WDAY_N/strided_slice/stack?
WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_1?
WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_2?
WDAY_N/strided_sliceStridedSliceWDAY_N/Shape:output:0#WDAY_N/strided_slice/stack:output:0%WDAY_N/strided_slice/stack_1:output:0%WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WDAY_N/strided_slicer
WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WDAY_N/Reshape/shape/1?
WDAY_N/Reshape/shapePackWDAY_N/strided_slice:output:0WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WDAY_N/Reshape/shape?
WDAY_N/ReshapeReshapeWDAY_N/truediv:z:0WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/Reshapeu
WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WEEK/ExpandDims/dim?
WEEK/ExpandDims
ExpandDimsfeatures_weekWEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WEEK/ExpandDims_
WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
WEEK/Cast/x}
WEEK/SubSubWEEK/ExpandDims:output:0WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WEEK/Subc
WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2
WEEK/Cast_1/x
WEEK/truedivRealDivWEEK/Sub:z:0WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WEEK/truedivX

WEEK/ShapeShapeWEEK/truediv:z:0*
T0*
_output_shapes
:2

WEEK/Shape~
WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WEEK/strided_slice/stack?
WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_1?
WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_2?
WEEK/strided_sliceStridedSliceWEEK/Shape:output:0!WEEK/strided_slice/stack:output:0#WEEK/strided_slice/stack_1:output:0#WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WEEK/strided_slicen
WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WEEK/Reshape/shape/1?
WEEK/Reshape/shapePackWEEK/strided_slice:output:0WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WEEK/Reshape/shape?
WEEK/ReshapeReshapeWEEK/truediv:z:0WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WEEK/Reshapeu
YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
YEAR/ExpandDims/dim?
YEAR/ExpandDims
ExpandDimsfeatures_yearYEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
YEAR/ExpandDims_
YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
YEAR/Cast/x}
YEAR/SubSubYEAR/ExpandDims:output:0YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

YEAR/Subc
YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2
YEAR/Cast_1/x
YEAR/truedivRealDivYEAR/Sub:z:0YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
YEAR/truedivX

YEAR/ShapeShapeYEAR/truediv:z:0*
T0*
_output_shapes
:2

YEAR/Shape~
YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
YEAR/strided_slice/stack?
YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_1?
YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_2?
YEAR/strided_sliceStridedSliceYEAR/Shape:output:0!YEAR/strided_slice/stack:output:0#YEAR/strided_slice/stack_1:output:0#YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
YEAR/strided_slicen
YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
YEAR/Reshape/shape/1?
YEAR/Reshape/shapePackYEAR/strided_slice:output:0YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
YEAR/Reshape/shape?
YEAR/ReshapeReshapeYEAR/truediv:z:0YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
YEAR/Reshapee
concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
concat/axis?
concatConcatV2AVAILABLE/Reshape:output:0GROUP_N/Reshape:output:0#TOTAL_APPLICATIONS/Reshape:output:0WDAY_N/Reshape:output:0WEEK/Reshape:output:0YEAR/Reshape:output:0concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
concatc
IdentityIdentityconcat:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:W S
#
_output_shapes
:?????????
,
_user_specified_namefeatures/AVAILABLE:UQ
#
_output_shapes
:?????????
*
_user_specified_namefeatures/GROUP_N:`\
#
_output_shapes
:?????????
5
_user_specified_namefeatures/TOTAL_APPLICATIONS:TP
#
_output_shapes
:?????????
)
_user_specified_namefeatures/WDAY_N:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/WEEK:RN
#
_output_shapes
:?????????
'
_user_specified_namefeatures/YEAR
?i
?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_22856
features

features_1

features_2

features_3

features_4

features_5
identity
AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
AVAILABLE/ExpandDims/dim?
AVAILABLE/ExpandDims
ExpandDimsfeatures!AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/ExpandDimsi
AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2
AVAILABLE/Cast/x?
AVAILABLE/SubSubAVAILABLE/ExpandDims:output:0AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Subm
AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2
AVAILABLE/Cast_1/x?
AVAILABLE/truedivRealDivAVAILABLE/Sub:z:0AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/truedivg
AVAILABLE/ShapeShapeAVAILABLE/truediv:z:0*
T0*
_output_shapes
:2
AVAILABLE/Shape?
AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
AVAILABLE/strided_slice/stack?
AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_1?
AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_2?
AVAILABLE/strided_sliceStridedSliceAVAILABLE/Shape:output:0&AVAILABLE/strided_slice/stack:output:0(AVAILABLE/strided_slice/stack_1:output:0(AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
AVAILABLE/strided_slicex
AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
AVAILABLE/Reshape/shape/1?
AVAILABLE/Reshape/shapePack AVAILABLE/strided_slice:output:0"AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
AVAILABLE/Reshape/shape?
AVAILABLE/ReshapeReshapeAVAILABLE/truediv:z:0 AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Reshape{
GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
GROUP_N/ExpandDims/dim?
GROUP_N/ExpandDims
ExpandDims
features_1GROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/ExpandDimse
GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2
GROUP_N/Cast/x?
GROUP_N/SubSubGROUP_N/ExpandDims:output:0GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Subi
GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2
GROUP_N/Cast_1/x?
GROUP_N/truedivRealDivGROUP_N/Sub:z:0GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/truediva
GROUP_N/ShapeShapeGROUP_N/truediv:z:0*
T0*
_output_shapes
:2
GROUP_N/Shape?
GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
GROUP_N/strided_slice/stack?
GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_1?
GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_2?
GROUP_N/strided_sliceStridedSliceGROUP_N/Shape:output:0$GROUP_N/strided_slice/stack:output:0&GROUP_N/strided_slice/stack_1:output:0&GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
GROUP_N/strided_slicet
GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
GROUP_N/Reshape/shape/1?
GROUP_N/Reshape/shapePackGROUP_N/strided_slice:output:0 GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
GROUP_N/Reshape/shape?
GROUP_N/ReshapeReshapeGROUP_N/truediv:z:0GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Reshape?
!TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2#
!TOTAL_APPLICATIONS/ExpandDims/dim?
TOTAL_APPLICATIONS/ExpandDims
ExpandDims
features_2*TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/ExpandDims{
TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2
TOTAL_APPLICATIONS/Cast/x?
TOTAL_APPLICATIONS/SubSub&TOTAL_APPLICATIONS/ExpandDims:output:0"TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Sub
TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2
TOTAL_APPLICATIONS/Cast_1/x?
TOTAL_APPLICATIONS/truedivRealDivTOTAL_APPLICATIONS/Sub:z:0$TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/truediv?
TOTAL_APPLICATIONS/ShapeShapeTOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2
TOTAL_APPLICATIONS/Shape?
&TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&TOTAL_APPLICATIONS/strided_slice/stack?
(TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_1?
(TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_2?
 TOTAL_APPLICATIONS/strided_sliceStridedSlice!TOTAL_APPLICATIONS/Shape:output:0/TOTAL_APPLICATIONS/strided_slice/stack:output:01TOTAL_APPLICATIONS/strided_slice/stack_1:output:01TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 TOTAL_APPLICATIONS/strided_slice?
"TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2$
"TOTAL_APPLICATIONS/Reshape/shape/1?
 TOTAL_APPLICATIONS/Reshape/shapePack)TOTAL_APPLICATIONS/strided_slice:output:0+TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2"
 TOTAL_APPLICATIONS/Reshape/shape?
TOTAL_APPLICATIONS/ReshapeReshapeTOTAL_APPLICATIONS/truediv:z:0)TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Reshapey
WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WDAY_N/ExpandDims/dim?
WDAY_N/ExpandDims
ExpandDims
features_3WDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/ExpandDimsc
WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2
WDAY_N/Cast/x?

WDAY_N/SubSubWDAY_N/ExpandDims:output:0WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WDAY_N/Subg
WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2
WDAY_N/Cast_1/x?
WDAY_N/truedivRealDivWDAY_N/Sub:z:0WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/truediv^
WDAY_N/ShapeShapeWDAY_N/truediv:z:0*
T0*
_output_shapes
:2
WDAY_N/Shape?
WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WDAY_N/strided_slice/stack?
WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_1?
WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_2?
WDAY_N/strided_sliceStridedSliceWDAY_N/Shape:output:0#WDAY_N/strided_slice/stack:output:0%WDAY_N/strided_slice/stack_1:output:0%WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WDAY_N/strided_slicer
WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WDAY_N/Reshape/shape/1?
WDAY_N/Reshape/shapePackWDAY_N/strided_slice:output:0WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WDAY_N/Reshape/shape?
WDAY_N/ReshapeReshapeWDAY_N/truediv:z:0WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/Reshapeu
WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WEEK/ExpandDims/dim?
WEEK/ExpandDims
ExpandDims
features_4WEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WEEK/ExpandDims_
WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
WEEK/Cast/x}
WEEK/SubSubWEEK/ExpandDims:output:0WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WEEK/Subc
WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2
WEEK/Cast_1/x
WEEK/truedivRealDivWEEK/Sub:z:0WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WEEK/truedivX

WEEK/ShapeShapeWEEK/truediv:z:0*
T0*
_output_shapes
:2

WEEK/Shape~
WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WEEK/strided_slice/stack?
WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_1?
WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_2?
WEEK/strided_sliceStridedSliceWEEK/Shape:output:0!WEEK/strided_slice/stack:output:0#WEEK/strided_slice/stack_1:output:0#WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WEEK/strided_slicen
WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WEEK/Reshape/shape/1?
WEEK/Reshape/shapePackWEEK/strided_slice:output:0WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WEEK/Reshape/shape?
WEEK/ReshapeReshapeWEEK/truediv:z:0WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WEEK/Reshapeu
YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
YEAR/ExpandDims/dim?
YEAR/ExpandDims
ExpandDims
features_5YEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
YEAR/ExpandDims_
YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
YEAR/Cast/x}
YEAR/SubSubYEAR/ExpandDims:output:0YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

YEAR/Subc
YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2
YEAR/Cast_1/x
YEAR/truedivRealDivYEAR/Sub:z:0YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
YEAR/truedivX

YEAR/ShapeShapeYEAR/truediv:z:0*
T0*
_output_shapes
:2

YEAR/Shape~
YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
YEAR/strided_slice/stack?
YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_1?
YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_2?
YEAR/strided_sliceStridedSliceYEAR/Shape:output:0!YEAR/strided_slice/stack:output:0#YEAR/strided_slice/stack_1:output:0#YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
YEAR/strided_slicen
YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
YEAR/Reshape/shape/1?
YEAR/Reshape/shapePackYEAR/strided_slice:output:0YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
YEAR/Reshape/shape?
YEAR/ReshapeReshapeYEAR/truediv:z:0YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
YEAR/Reshapee
concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
concat/axis?
concatConcatV2AVAILABLE/Reshape:output:0GROUP_N/Reshape:output:0#TOTAL_APPLICATIONS/Reshape:output:0WDAY_N/Reshape:output:0WEEK/Reshape:output:0YEAR/Reshape:output:0concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
concatc
IdentityIdentityconcat:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:M I
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features
?	
?
B__inference_dense_3_layer_call_and_return_conditional_losses_23646

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdd?
IdentityIdentityBiasAdd:output:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
|
'__inference_dense_4_layer_call_fn_23636

inputs
unknown
	unknown_0
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_4_layer_call_and_return_conditional_losses_229172
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?i
?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_22761
features

features_1

features_2

features_3

features_4

features_5
identity
AVAILABLE/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
AVAILABLE/ExpandDims/dim?
AVAILABLE/ExpandDims
ExpandDimsfeatures!AVAILABLE/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/ExpandDimsi
AVAILABLE/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *??q@2
AVAILABLE/Cast/x?
AVAILABLE/SubSubAVAILABLE/ExpandDims:output:0AVAILABLE/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Subm
AVAILABLE/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??J?2
AVAILABLE/Cast_1/x?
AVAILABLE/truedivRealDivAVAILABLE/Sub:z:0AVAILABLE/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/truedivg
AVAILABLE/ShapeShapeAVAILABLE/truediv:z:0*
T0*
_output_shapes
:2
AVAILABLE/Shape?
AVAILABLE/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
AVAILABLE/strided_slice/stack?
AVAILABLE/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_1?
AVAILABLE/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
AVAILABLE/strided_slice/stack_2?
AVAILABLE/strided_sliceStridedSliceAVAILABLE/Shape:output:0&AVAILABLE/strided_slice/stack:output:0(AVAILABLE/strided_slice/stack_1:output:0(AVAILABLE/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
AVAILABLE/strided_slicex
AVAILABLE/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
AVAILABLE/Reshape/shape/1?
AVAILABLE/Reshape/shapePack AVAILABLE/strided_slice:output:0"AVAILABLE/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
AVAILABLE/Reshape/shape?
AVAILABLE/ReshapeReshapeAVAILABLE/truediv:z:0 AVAILABLE/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
AVAILABLE/Reshape{
GROUP_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
GROUP_N/ExpandDims/dim?
GROUP_N/ExpandDims
ExpandDims
features_1GROUP_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/ExpandDimse
GROUP_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *<$?@2
GROUP_N/Cast/x?
GROUP_N/SubSubGROUP_N/ExpandDims:output:0GROUP_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Subi
GROUP_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *q78@2
GROUP_N/Cast_1/x?
GROUP_N/truedivRealDivGROUP_N/Sub:z:0GROUP_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/truediva
GROUP_N/ShapeShapeGROUP_N/truediv:z:0*
T0*
_output_shapes
:2
GROUP_N/Shape?
GROUP_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
GROUP_N/strided_slice/stack?
GROUP_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_1?
GROUP_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
GROUP_N/strided_slice/stack_2?
GROUP_N/strided_sliceStridedSliceGROUP_N/Shape:output:0$GROUP_N/strided_slice/stack:output:0&GROUP_N/strided_slice/stack_1:output:0&GROUP_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
GROUP_N/strided_slicet
GROUP_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
GROUP_N/Reshape/shape/1?
GROUP_N/Reshape/shapePackGROUP_N/strided_slice:output:0 GROUP_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
GROUP_N/Reshape/shape?
GROUP_N/ReshapeReshapeGROUP_N/truediv:z:0GROUP_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
GROUP_N/Reshape?
!TOTAL_APPLICATIONS/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2#
!TOTAL_APPLICATIONS/ExpandDims/dim?
TOTAL_APPLICATIONS/ExpandDims
ExpandDims
features_2*TOTAL_APPLICATIONS/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/ExpandDims{
TOTAL_APPLICATIONS/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?N?C2
TOTAL_APPLICATIONS/Cast/x?
TOTAL_APPLICATIONS/SubSub&TOTAL_APPLICATIONS/ExpandDims:output:0"TOTAL_APPLICATIONS/Cast/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Sub
TOTAL_APPLICATIONS/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *??C2
TOTAL_APPLICATIONS/Cast_1/x?
TOTAL_APPLICATIONS/truedivRealDivTOTAL_APPLICATIONS/Sub:z:0$TOTAL_APPLICATIONS/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/truediv?
TOTAL_APPLICATIONS/ShapeShapeTOTAL_APPLICATIONS/truediv:z:0*
T0*
_output_shapes
:2
TOTAL_APPLICATIONS/Shape?
&TOTAL_APPLICATIONS/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&TOTAL_APPLICATIONS/strided_slice/stack?
(TOTAL_APPLICATIONS/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_1?
(TOTAL_APPLICATIONS/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(TOTAL_APPLICATIONS/strided_slice/stack_2?
 TOTAL_APPLICATIONS/strided_sliceStridedSlice!TOTAL_APPLICATIONS/Shape:output:0/TOTAL_APPLICATIONS/strided_slice/stack:output:01TOTAL_APPLICATIONS/strided_slice/stack_1:output:01TOTAL_APPLICATIONS/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 TOTAL_APPLICATIONS/strided_slice?
"TOTAL_APPLICATIONS/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2$
"TOTAL_APPLICATIONS/Reshape/shape/1?
 TOTAL_APPLICATIONS/Reshape/shapePack)TOTAL_APPLICATIONS/strided_slice:output:0+TOTAL_APPLICATIONS/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2"
 TOTAL_APPLICATIONS/Reshape/shape?
TOTAL_APPLICATIONS/ReshapeReshapeTOTAL_APPLICATIONS/truediv:z:0)TOTAL_APPLICATIONS/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
TOTAL_APPLICATIONS/Reshapey
WDAY_N/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WDAY_N/ExpandDims/dim?
WDAY_N/ExpandDims
ExpandDims
features_3WDAY_N/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/ExpandDimsc
WDAY_N/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *ې@2
WDAY_N/Cast/x?

WDAY_N/SubSubWDAY_N/ExpandDims:output:0WDAY_N/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WDAY_N/Subg
WDAY_N/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *];@2
WDAY_N/Cast_1/x?
WDAY_N/truedivRealDivWDAY_N/Sub:z:0WDAY_N/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/truediv^
WDAY_N/ShapeShapeWDAY_N/truediv:z:0*
T0*
_output_shapes
:2
WDAY_N/Shape?
WDAY_N/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WDAY_N/strided_slice/stack?
WDAY_N/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_1?
WDAY_N/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WDAY_N/strided_slice/stack_2?
WDAY_N/strided_sliceStridedSliceWDAY_N/Shape:output:0#WDAY_N/strided_slice/stack:output:0%WDAY_N/strided_slice/stack_1:output:0%WDAY_N/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WDAY_N/strided_slicer
WDAY_N/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WDAY_N/Reshape/shape/1?
WDAY_N/Reshape/shapePackWDAY_N/strided_slice:output:0WDAY_N/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WDAY_N/Reshape/shape?
WDAY_N/ReshapeReshapeWDAY_N/truediv:z:0WDAY_N/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WDAY_N/Reshapeu
WEEK/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
WEEK/ExpandDims/dim?
WEEK/ExpandDims
ExpandDims
features_4WEEK/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
WEEK/ExpandDims_
WEEK/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *?A2
WEEK/Cast/x}
WEEK/SubSubWEEK/ExpandDims:output:0WEEK/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

WEEK/Subc
WEEK/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *F?@2
WEEK/Cast_1/x
WEEK/truedivRealDivWEEK/Sub:z:0WEEK/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
WEEK/truedivX

WEEK/ShapeShapeWEEK/truediv:z:0*
T0*
_output_shapes
:2

WEEK/Shape~
WEEK/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
WEEK/strided_slice/stack?
WEEK/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_1?
WEEK/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
WEEK/strided_slice/stack_2?
WEEK/strided_sliceStridedSliceWEEK/Shape:output:0!WEEK/strided_slice/stack:output:0#WEEK/strided_slice/stack_1:output:0#WEEK/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
WEEK/strided_slicen
WEEK/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
WEEK/Reshape/shape/1?
WEEK/Reshape/shapePackWEEK/strided_slice:output:0WEEK/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
WEEK/Reshape/shape?
WEEK/ReshapeReshapeWEEK/truediv:z:0WEEK/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
WEEK/Reshapeu
YEAR/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
YEAR/ExpandDims/dim?
YEAR/ExpandDims
ExpandDims
features_5YEAR/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
YEAR/ExpandDims_
YEAR/Cast/xConst*
_output_shapes
: *
dtype0*
valueB
 *? ?D2
YEAR/Cast/x}
YEAR/SubSubYEAR/ExpandDims:output:0YEAR/Cast/x:output:0*
T0*'
_output_shapes
:?????????2

YEAR/Subc
YEAR/Cast_1/xConst*
_output_shapes
: *
dtype0*
valueB
 *1[J@2
YEAR/Cast_1/x
YEAR/truedivRealDivYEAR/Sub:z:0YEAR/Cast_1/x:output:0*
T0*'
_output_shapes
:?????????2
YEAR/truedivX

YEAR/ShapeShapeYEAR/truediv:z:0*
T0*
_output_shapes
:2

YEAR/Shape~
YEAR/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
YEAR/strided_slice/stack?
YEAR/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_1?
YEAR/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
YEAR/strided_slice/stack_2?
YEAR/strided_sliceStridedSliceYEAR/Shape:output:0!YEAR/strided_slice/stack:output:0#YEAR/strided_slice/stack_1:output:0#YEAR/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
YEAR/strided_slicen
YEAR/Reshape/shape/1Const*
_output_shapes
: *
dtype0*
value	B :2
YEAR/Reshape/shape/1?
YEAR/Reshape/shapePackYEAR/strided_slice:output:0YEAR/Reshape/shape/1:output:0*
N*
T0*
_output_shapes
:2
YEAR/Reshape/shape?
YEAR/ReshapeReshapeYEAR/truediv:z:0YEAR/Reshape/shape:output:0*
T0*'
_output_shapes
:?????????2
YEAR/Reshapee
concat/axisConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
concat/axis?
concatConcatV2AVAILABLE/Reshape:output:0GROUP_N/Reshape:output:0#TOTAL_APPLICATIONS/Reshape:output:0WDAY_N/Reshape:output:0WEEK/Reshape:output:0YEAR/Reshape:output:0concat/axis:output:0*
N*
T0*'
_output_shapes
:?????????2
concatc
IdentityIdentityconcat:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*m
_input_shapes\
Z:?????????:?????????:?????????:?????????:?????????:?????????:M I
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features:MI
#
_output_shapes
:?????????
"
_user_specified_name
features
?
?
B__inference_model_1_layer_call_and_return_conditional_losses_23065

inputs
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
dense_5_23049
dense_5_23051
dense_4_23054
dense_4_23056
dense_3_23059
dense_3_23061
identity??dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?
 dense_features_1/PartitionedCallPartitionedCallinputsinputs_1inputs_2inputs_3inputs_4inputs_5*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_228562"
 dense_features_1/PartitionedCall?
dense_5/StatefulPartitionedCallStatefulPartitionedCall)dense_features_1/PartitionedCall:output:0dense_5_23049dense_5_23051*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_5_layer_call_and_return_conditional_losses_228902!
dense_5/StatefulPartitionedCall?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dense_5/StatefulPartitionedCall:output:0dense_4_23054dense_4_23056*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_4_layer_call_and_return_conditional_losses_229172!
dense_4/StatefulPartitionedCall?
dense_3/StatefulPartitionedCallStatefulPartitionedCall(dense_4/StatefulPartitionedCall:output:0dense_3_23059dense_3_23061*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_229432!
dense_3/StatefulPartitionedCall?
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0 ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall:K G
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?
|
'__inference_dense_5_layer_call_fn_23616

inputs
unknown
	unknown_0
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_5_layer_call_and_return_conditional_losses_228902
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
B__inference_dense_4_layer_call_and_return_conditional_losses_23627

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relu?
IdentityIdentityRelu:activations:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?	
?
B__inference_dense_3_layer_call_and_return_conditional_losses_22943

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdd?
IdentityIdentityBiasAdd:output:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
?
B__inference_model_1_layer_call_and_return_conditional_losses_23018

inputs
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
dense_5_23002
dense_5_23004
dense_4_23007
dense_4_23009
dense_3_23012
dense_3_23014
identity??dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?
 dense_features_1/PartitionedCallPartitionedCallinputsinputs_1inputs_2inputs_3inputs_4inputs_5*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *T
fORM
K__inference_dense_features_1_layer_call_and_return_conditional_losses_227612"
 dense_features_1/PartitionedCall?
dense_5/StatefulPartitionedCallStatefulPartitionedCall)dense_features_1/PartitionedCall:output:0dense_5_23002dense_5_23004*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_5_layer_call_and_return_conditional_losses_228902!
dense_5/StatefulPartitionedCall?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dense_5/StatefulPartitionedCall:output:0dense_4_23007dense_4_23009*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_4_layer_call_and_return_conditional_losses_229172!
dense_4/StatefulPartitionedCall?
dense_3/StatefulPartitionedCallStatefulPartitionedCall(dense_4/StatefulPartitionedCall:output:0dense_3_23012dense_3_23014*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_229432!
dense_3/StatefulPartitionedCall?
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0 ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapest
r:?????????:?????????:?????????:?????????:?????????:?????????::::::2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall:K G
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
B__inference_dense_4_layer_call_and_return_conditional_losses_22917

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relu?
IdentityIdentityRelu:activations:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????@::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs"?L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
;
	AVAILABLE.
serving_default_AVAILABLE:0?????????
7
GROUP_N,
serving_default_GROUP_N:0?????????
M
TOTAL_APPLICATIONS7
$serving_default_TOTAL_APPLICATIONS:0?????????
5
WDAY_N+
serving_default_WDAY_N:0?????????
1
WEEK)
serving_default_WEEK:0?????????
1
YEAR)
serving_default_YEAR:0?????????;
dense_30
StatefulPartitionedCall:0?????????tensorflow/serving/predict:??
?]
layer-0
layer-1
layer-2
layer-3
layer-4
layer-5
layer-6
layer_with_weights-0
layer-7
	layer_with_weights-1
	layer-8

layer_with_weights-2

layer-9
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
*X&call_and_return_all_conditional_losses
Y__call__
Z_default_save_signature"?Z
_tf_keras_network?Z{"class_name": "Functional", "name": "model_1", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "must_restore_from_config": false, "config": {"name": "model_1", "layers": [{"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "AVAILABLE"}, "name": "AVAILABLE", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "GROUP_N"}, "name": "GROUP_N", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "TOTAL_APPLICATIONS"}, "name": "TOTAL_APPLICATIONS", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WDAY_N"}, "name": "WDAY_N", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WEEK"}, "name": "WEEK", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "YEAR"}, "name": "YEAR", "inbound_nodes": []}, {"class_name": "DenseFeatures", "config": {"name": "dense_features_1", "trainable": true, "dtype": "float32", "feature_columns": [{"class_name": "NumericColumn", "config": {"key": "AVAILABLE", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "GROUP_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "TOTAL_APPLICATIONS", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WDAY_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WEEK", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "YEAR", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}], "partitioner": null}, "name": "dense_features_1", "inbound_nodes": [{"YEAR": ["YEAR", 0, 0, {}], "WEEK": ["WEEK", 0, 0, {}], "WDAY_N": ["WDAY_N", 0, 0, {}], "AVAILABLE": ["AVAILABLE", 0, 0, {}], "TOTAL_APPLICATIONS": ["TOTAL_APPLICATIONS", 0, 0, {}], "GROUP_N": ["GROUP_N", 0, 0, {}]}]}, {"class_name": "Dense", "config": {"name": "dense_5", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_5", "inbound_nodes": [[["dense_features_1", 0, 0, {}]]]}, {"class_name": "Dense", "config": {"name": "dense_4", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_4", "inbound_nodes": [[["dense_5", 0, 0, {}]]]}, {"class_name": "Dense", "config": {"name": "dense_3", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_3", "inbound_nodes": [[["dense_4", 0, 0, {}]]]}], "input_layers": {"YEAR": ["YEAR", 0, 0], "WEEK": ["WEEK", 0, 0], "WDAY_N": ["WDAY_N", 0, 0], "AVAILABLE": ["AVAILABLE", 0, 0], "TOTAL_APPLICATIONS": ["TOTAL_APPLICATIONS", 0, 0], "GROUP_N": ["GROUP_N", 0, 0]}, "output_layers": [["dense_3", 0, 0]]}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}, {"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}, {"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}, {"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}, {"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}, {"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null]}, "ndim": 1, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"YEAR": {"class_name": "TensorShape", "items": [null]}, "WEEK": {"class_name": "TensorShape", "items": [null]}, "WDAY_N": {"class_name": "TensorShape", "items": [null]}, "AVAILABLE": {"class_name": "TensorShape", "items": [null]}, "TOTAL_APPLICATIONS": {"class_name": "TensorShape", "items": [null]}, "GROUP_N": {"class_name": "TensorShape", "items": [null]}}, "is_graph_network": true, "keras_version": "2.4.0", "backend": "tensorflow", "model_config": {"class_name": "Functional", "config": {"name": "model_1", "layers": [{"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "AVAILABLE"}, "name": "AVAILABLE", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "GROUP_N"}, "name": "GROUP_N", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "TOTAL_APPLICATIONS"}, "name": "TOTAL_APPLICATIONS", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WDAY_N"}, "name": "WDAY_N", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WEEK"}, "name": "WEEK", "inbound_nodes": []}, {"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "YEAR"}, "name": "YEAR", "inbound_nodes": []}, {"class_name": "DenseFeatures", "config": {"name": "dense_features_1", "trainable": true, "dtype": "float32", "feature_columns": [{"class_name": "NumericColumn", "config": {"key": "AVAILABLE", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "GROUP_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "TOTAL_APPLICATIONS", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WDAY_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WEEK", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "YEAR", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}], "partitioner": null}, "name": "dense_features_1", "inbound_nodes": [{"YEAR": ["YEAR", 0, 0, {}], "WEEK": ["WEEK", 0, 0, {}], "WDAY_N": ["WDAY_N", 0, 0, {}], "AVAILABLE": ["AVAILABLE", 0, 0, {}], "TOTAL_APPLICATIONS": ["TOTAL_APPLICATIONS", 0, 0, {}], "GROUP_N": ["GROUP_N", 0, 0, {}]}]}, {"class_name": "Dense", "config": {"name": "dense_5", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_5", "inbound_nodes": [[["dense_features_1", 0, 0, {}]]]}, {"class_name": "Dense", "config": {"name": "dense_4", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_4", "inbound_nodes": [[["dense_5", 0, 0, {}]]]}, {"class_name": "Dense", "config": {"name": "dense_3", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "name": "dense_3", "inbound_nodes": [[["dense_4", 0, 0, {}]]]}], "input_layers": {"YEAR": ["YEAR", 0, 0], "WEEK": ["WEEK", 0, 0], "WDAY_N": ["WDAY_N", 0, 0], "AVAILABLE": ["AVAILABLE", 0, 0], "TOTAL_APPLICATIONS": ["TOTAL_APPLICATIONS", 0, 0], "GROUP_N": ["GROUP_N", 0, 0]}, "output_layers": [["dense_3", 0, 0]]}}, "training_config": {"loss": "mse", "metrics": [[{"class_name": "MeanMetricWrapper", "config": {"name": "mean_absolute_error", "dtype": "float32", "fn": "mean_absolute_error"}}]], "weighted_metrics": null, "loss_weights": null, "optimizer_config": {"class_name": "RMSprop", "config": {"name": "RMSprop", "learning_rate": 0.0010000000474974513, "decay": 0.0, "rho": 0.8999999761581421, "momentum": 0.0, "epsilon": 1e-07, "centered": false}}}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "AVAILABLE", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "AVAILABLE"}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "GROUP_N", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "GROUP_N"}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "TOTAL_APPLICATIONS", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "TOTAL_APPLICATIONS"}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "WDAY_N", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WDAY_N"}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "WEEK", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "WEEK"}}
?"?
_tf_keras_input_layer?{"class_name": "InputLayer", "name": "YEAR", "dtype": "float32", "sparse": false, "ragged": false, "batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "YEAR"}}
?
_feature_columns

_resources
	variables
trainable_variables
regularization_losses
	keras_api
*[&call_and_return_all_conditional_losses
\__call__"?
_tf_keras_layer?{"class_name": "DenseFeatures", "name": "dense_features_1", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "dense_features_1", "trainable": true, "dtype": "float32", "feature_columns": [{"class_name": "NumericColumn", "config": {"key": "AVAILABLE", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "GROUP_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "TOTAL_APPLICATIONS", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WDAY_N", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "WEEK", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}, {"class_name": "NumericColumn", "config": {"key": "YEAR", "shape": {"class_name": "__tuple__", "items": [1]}, "default_value": null, "dtype": "float32", "normalizer_fn": "python_function"}}], "partitioner": null}, "build_input_shape": {"YEAR": {"class_name": "TensorShape", "items": [null]}, "WEEK": {"class_name": "TensorShape", "items": [null]}, "WDAY_N": {"class_name": "TensorShape", "items": [null]}, "AVAILABLE": {"class_name": "TensorShape", "items": [null]}, "TOTAL_APPLICATIONS": {"class_name": "TensorShape", "items": [null]}, "GROUP_N": {"class_name": "TensorShape", "items": [null]}}, "_is_feature_layer": true}
?

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
*]&call_and_return_all_conditional_losses
^__call__"?
_tf_keras_layer?{"class_name": "Dense", "name": "dense_5", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "dense_5", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 6}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 6]}}
?

kernel
bias
	variables
 trainable_variables
!regularization_losses
"	keras_api
*_&call_and_return_all_conditional_losses
`__call__"?
_tf_keras_layer?{"class_name": "Dense", "name": "dense_4", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "dense_4", "trainable": true, "dtype": "float32", "units": 64, "activation": "relu", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 64}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 64]}}
?

#kernel
$bias
%	variables
&trainable_variables
'regularization_losses
(	keras_api
*a&call_and_return_all_conditional_losses
b__call__"?
_tf_keras_layer?{"class_name": "Dense", "name": "dense_3", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "dense_3", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 64}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 64]}}
?
)iter
	*decay
+learning_rate
,momentum
-rho	rmsR	rmsS	rmsT	rmsU	#rmsV	$rmsW"
	optimizer
J
0
1
2
3
#4
$5"
trackable_list_wrapper
J
0
1
2
3
#4
$5"
trackable_list_wrapper
 "
trackable_list_wrapper
?
.layer_metrics
	variables
trainable_variables
/layer_regularization_losses
0metrics
1non_trainable_variables
regularization_losses

2layers
Y__call__
Z_default_save_signature
*X&call_and_return_all_conditional_losses
&X"call_and_return_conditional_losses"
_generic_user_object
,
cserving_default"
signature_map
 "
trackable_list_wrapper
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
3layer_metrics
	variables
trainable_variables
4layer_regularization_losses
5metrics
regularization_losses
6non_trainable_variables

7layers
\__call__
*[&call_and_return_all_conditional_losses
&["call_and_return_conditional_losses"
_generic_user_object
 :@2dense_5/kernel
:@2dense_5/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
8layer_metrics
	variables
trainable_variables
9layer_regularization_losses
:metrics
regularization_losses
;non_trainable_variables

<layers
^__call__
*]&call_and_return_all_conditional_losses
&]"call_and_return_conditional_losses"
_generic_user_object
 :@@2dense_4/kernel
:@2dense_4/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
=layer_metrics
	variables
 trainable_variables
>layer_regularization_losses
?metrics
!regularization_losses
@non_trainable_variables

Alayers
`__call__
*_&call_and_return_all_conditional_losses
&_"call_and_return_conditional_losses"
_generic_user_object
 :@2dense_3/kernel
:2dense_3/bias
.
#0
$1"
trackable_list_wrapper
.
#0
$1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Blayer_metrics
%	variables
&trainable_variables
Clayer_regularization_losses
Dmetrics
'regularization_losses
Enon_trainable_variables

Flayers
b__call__
*a&call_and_return_all_conditional_losses
&a"call_and_return_conditional_losses"
_generic_user_object
:	 (2RMSprop/iter
: (2RMSprop/decay
: (2RMSprop/learning_rate
: (2RMSprop/momentum
: (2RMSprop/rho
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
.
G0
H1"
trackable_list_wrapper
 "
trackable_list_wrapper
f
0
1
2
3
4
5
6
7
	8

9"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
	Itotal
	Jcount
K	variables
L	keras_api"?
_tf_keras_metricj{"class_name": "Mean", "name": "loss", "dtype": "float32", "config": {"name": "loss", "dtype": "float32"}}
?
	Mtotal
	Ncount
O
_fn_kwargs
P	variables
Q	keras_api"?
_tf_keras_metric?{"class_name": "MeanMetricWrapper", "name": "mean_absolute_error", "dtype": "float32", "config": {"name": "mean_absolute_error", "dtype": "float32", "fn": "mean_absolute_error"}}
:  (2total
:  (2count
.
I0
J1"
trackable_list_wrapper
-
K	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
M0
N1"
trackable_list_wrapper
-
P	variables"
_generic_user_object
*:(@2RMSprop/dense_5/kernel/rms
$:"@2RMSprop/dense_5/bias/rms
*:(@@2RMSprop/dense_4/kernel/rms
$:"@2RMSprop/dense_4/bias/rms
*:(@2RMSprop/dense_3/kernel/rms
$:"2RMSprop/dense_3/bias/rms
?2?
B__inference_model_1_layer_call_and_return_conditional_losses_23227
B__inference_model_1_layer_call_and_return_conditional_losses_22960
B__inference_model_1_layer_call_and_return_conditional_losses_22985
B__inference_model_1_layer_call_and_return_conditional_losses_23342?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
'__inference_model_1_layer_call_fn_23080
'__inference_model_1_layer_call_fn_23386
'__inference_model_1_layer_call_fn_23364
'__inference_model_1_layer_call_fn_23033?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
 __inference__wrapped_model_22657?
???
FullArgSpec
args? 
varargsjargs
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
?2?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23481
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23576?
???
FullArgSpecE
args=?:
jself

jfeatures
jcols_to_output_tensors

jtraining
varargs
 
varkw
 
defaults?

 
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
0__inference_dense_features_1_layer_call_fn_23596
0__inference_dense_features_1_layer_call_fn_23586?
???
FullArgSpecE
args=?:
jself

jfeatures
jcols_to_output_tensors

jtraining
varargs
 
varkw
 
defaults?

 
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
B__inference_dense_5_layer_call_and_return_conditional_losses_23607?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
'__inference_dense_5_layer_call_fn_23616?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
B__inference_dense_4_layer_call_and_return_conditional_losses_23627?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
'__inference_dense_4_layer_call_fn_23636?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
B__inference_dense_3_layer_call_and_return_conditional_losses_23646?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
'__inference_dense_3_layer_call_fn_23655?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
#__inference_signature_wrapper_23112	AVAILABLEGROUP_NTOTAL_APPLICATIONSWDAY_NWEEKYEAR"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 ?
 __inference__wrapped_model_22657?#$???
???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
? "1?.
,
dense_3!?
dense_3??????????
B__inference_dense_3_layer_call_and_return_conditional_losses_23646\#$/?,
%?"
 ?
inputs?????????@
? "%?"
?
0?????????
? z
'__inference_dense_3_layer_call_fn_23655O#$/?,
%?"
 ?
inputs?????????@
? "???????????
B__inference_dense_4_layer_call_and_return_conditional_losses_23627\/?,
%?"
 ?
inputs?????????@
? "%?"
?
0?????????@
? z
'__inference_dense_4_layer_call_fn_23636O/?,
%?"
 ?
inputs?????????@
? "??????????@?
B__inference_dense_5_layer_call_and_return_conditional_losses_23607\/?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????@
? z
'__inference_dense_5_layer_call_fn_23616O/?,
%?"
 ?
inputs?????????
? "??????????@?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23481????
???
???
5
	AVAILABLE(?%
features/AVAILABLE?????????
1
GROUP_N&?#
features/GROUP_N?????????
G
TOTAL_APPLICATIONS1?.
features/TOTAL_APPLICATIONS?????????
/
WDAY_N%?"
features/WDAY_N?????????
+
WEEK#? 
features/WEEK?????????
+
YEAR#? 
features/YEAR?????????

 
p
? "%?"
?
0?????????
? ?
K__inference_dense_features_1_layer_call_and_return_conditional_losses_23576????
???
???
5
	AVAILABLE(?%
features/AVAILABLE?????????
1
GROUP_N&?#
features/GROUP_N?????????
G
TOTAL_APPLICATIONS1?.
features/TOTAL_APPLICATIONS?????????
/
WDAY_N%?"
features/WDAY_N?????????
+
WEEK#? 
features/WEEK?????????
+
YEAR#? 
features/YEAR?????????

 
p 
? "%?"
?
0?????????
? ?
0__inference_dense_features_1_layer_call_fn_23586????
???
???
5
	AVAILABLE(?%
features/AVAILABLE?????????
1
GROUP_N&?#
features/GROUP_N?????????
G
TOTAL_APPLICATIONS1?.
features/TOTAL_APPLICATIONS?????????
/
WDAY_N%?"
features/WDAY_N?????????
+
WEEK#? 
features/WEEK?????????
+
YEAR#? 
features/YEAR?????????

 
p
? "???????????
0__inference_dense_features_1_layer_call_fn_23596????
???
???
5
	AVAILABLE(?%
features/AVAILABLE?????????
1
GROUP_N&?#
features/GROUP_N?????????
G
TOTAL_APPLICATIONS1?.
features/TOTAL_APPLICATIONS?????????
/
WDAY_N%?"
features/WDAY_N?????????
+
WEEK#? 
features/WEEK?????????
+
YEAR#? 
features/YEAR?????????

 
p 
? "???????????
B__inference_model_1_layer_call_and_return_conditional_losses_22960?#$???
???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
p

 
? "%?"
?
0?????????
? ?
B__inference_model_1_layer_call_and_return_conditional_losses_22985?#$???
???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
p 

 
? "%?"
?
0?????????
? ?
B__inference_model_1_layer_call_and_return_conditional_losses_23227?#$???
???
???
3
	AVAILABLE&?#
inputs/AVAILABLE?????????
/
GROUP_N$?!
inputs/GROUP_N?????????
E
TOTAL_APPLICATIONS/?,
inputs/TOTAL_APPLICATIONS?????????
-
WDAY_N#? 
inputs/WDAY_N?????????
)
WEEK!?
inputs/WEEK?????????
)
YEAR!?
inputs/YEAR?????????
p

 
? "%?"
?
0?????????
? ?
B__inference_model_1_layer_call_and_return_conditional_losses_23342?#$???
???
???
3
	AVAILABLE&?#
inputs/AVAILABLE?????????
/
GROUP_N$?!
inputs/GROUP_N?????????
E
TOTAL_APPLICATIONS/?,
inputs/TOTAL_APPLICATIONS?????????
-
WDAY_N#? 
inputs/WDAY_N?????????
)
WEEK!?
inputs/WEEK?????????
)
YEAR!?
inputs/YEAR?????????
p 

 
? "%?"
?
0?????????
? ?
'__inference_model_1_layer_call_fn_23033?#$???
???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
p

 
? "???????????
'__inference_model_1_layer_call_fn_23080?#$???
???
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????
p 

 
? "???????????
'__inference_model_1_layer_call_fn_23364?#$???
???
???
3
	AVAILABLE&?#
inputs/AVAILABLE?????????
/
GROUP_N$?!
inputs/GROUP_N?????????
E
TOTAL_APPLICATIONS/?,
inputs/TOTAL_APPLICATIONS?????????
-
WDAY_N#? 
inputs/WDAY_N?????????
)
WEEK!?
inputs/WEEK?????????
)
YEAR!?
inputs/YEAR?????????
p

 
? "???????????
'__inference_model_1_layer_call_fn_23386?#$???
???
???
3
	AVAILABLE&?#
inputs/AVAILABLE?????????
/
GROUP_N$?!
inputs/GROUP_N?????????
E
TOTAL_APPLICATIONS/?,
inputs/TOTAL_APPLICATIONS?????????
-
WDAY_N#? 
inputs/WDAY_N?????????
)
WEEK!?
inputs/WEEK?????????
)
YEAR!?
inputs/YEAR?????????
p 

 
? "???????????
#__inference_signature_wrapper_23112?#$???
? 
???
,
	AVAILABLE?
	AVAILABLE?????????
(
GROUP_N?
GROUP_N?????????
>
TOTAL_APPLICATIONS(?%
TOTAL_APPLICATIONS?????????
&
WDAY_N?
WDAY_N?????????
"
WEEK?
WEEK?????????
"
YEAR?
YEAR?????????"1?.
,
dense_3!?
dense_3?????????