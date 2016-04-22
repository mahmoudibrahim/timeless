clear all;

numTimePts = 3;
numTracks = 3;
numClusters = 11;
inFile = ['allFold.txt'];
inModelFile = ['model-' num2str(numClusters) '.mat'];
pathToBNT = ['pathToBNT']; %Define the Path to BNT here

%%%%define paths%%%
addpath(genpathKPM(pathToBNT));
%%%%%%%%%%%%%%%%%%%


%%%Prepare DAG structure%%%
inFile = ['allFold.txt'];
load(inModelFile);
numNodes = (numTimePts*numTracks) + 1;
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



%%%start inference%%%
disp(['Starting Inference...']);
marginal = zeros(numClusters, numDataPts);
for (nd = 1:numDataPts)
	evidence = cell(numNodes, 1);
	evidence(2:numNodes,1) = data(2:numNodes,nd);
	engine = jtree_inf_engine(tan);
	[engine, ll] = enter_evidence(engine, evidence');
	marg = marginal_nodes(engine, 1);
	[none, index] = max(marg.T);
	marginal(:,nd) = marg.T;
	data(1,nd) = num2cell(index); 
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%start inference%%%
disp(['Writing Results...']);
data = data';
classes = cell2num(data(:,1));
marginal = marginal';
outFile = ['classes-' num2str(numClusters) '.txt'];
classe = [classes, marginal];
dlmwrite(outFile, classe, '\t');
%%%%%%%%%%%%%%%%%%%%%%
