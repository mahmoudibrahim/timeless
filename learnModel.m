clear all;

numTimePts = 3;
numTracks = 3;
numClusters = 11;
inFile = ['allFold.txt'];
pathToBNT = ['pathToBNT']; %Define the Path to BNT here

%%%%define paths%%%
addpath(genpathKPM(pathToBNT));
%%%%%%%%%%%%%%%%%%%

%%%Prepare DAG structure%%%
numNodes = (numTimePts*numTracks) + 1;
dag = zeros(numNodes, numNodes);
dag(1, 2:numNodes) = 1;

dag(2,3) = 1;
dag(3,4) = 1;

dag(5,6) = 1;
dag(6,7) = 1;

dag(8,9) = 1;
dag(9,10) = 1;

discreteNodes = 1;
nodeSizes = ones(1,numNodes);
nodeSizes(1,1) = numClusters;
nodeSizes(1,2:numNodes) = 1;
tan = mk_bnet(dag, nodeSizes, 'discrete', discreteNodes, 'observed', [2:numNodes]);
tan.CPD{1} = tabular_CPD(tan, 1, 'CPT', 'unif', 'dirichlet_weight', 1, 'dirichlet_type', 'unif');
for(t = 2:numNodes)
	tan.CPD{t} = gaussian_CPD(tan, t);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%Import Data%%%
dataOrig = importdata(inFile, '\t');
numDataPts = length(dataOrig.data(:,1));
disp(['There are ' num2str(numDataPts) ' data points.']);
datee = dataOrig.data;
rem = num2cell(dataOrig.data,1);
rem = cell2num(rem);
data = cell(numDataPts, numNodes);
for (i = 1:numDataPts)
	data(i,2:numNodes) = num2cell(rem(i,:));
end
data = data';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%Initialize with k-means%%%
disp(['Initializing with K-means']);
k = kmeans(dataOrig.data, numClusters, 'Distance', 'cityblock', 'Replicates', 15, 'MaxIter', 300);
date = data;
date(1,:) = num2cell(k);
tan = learn_params(tan, date);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%EM%%%%
disp(['Started EM algorithm']);
engine = jtree_inf_engine(tan);
[tan, LLtrace, engine] = learn_params_em(engine, data, 200, 0.0002);
outFile = ['model-' num2str(numClusters) '.mat'];
save(outFile, 'tan');
numPar = 0;
for (i = 1:numNodes)
	temp = struct(tan.CPD{i});
	numPar = numPar + temp.nparams;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
