{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.SBV
import Data.Map (Map, fromList, elems, keys)
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative

--concrete IDs for VMs and servers
type VMID = Integer
type SID = Integer

--symbolic IDs servers
type SSID = SBV SID

--a type synonim for the location of VMs on servers
type VMLoc a = Map VMID a

--A VM is just a name and a cpuDemand
data VM = VM { vmName :: String,
               cpuDemand :: Integer}

--a server has got a name and a certain amount of free CPU
data Server = Server { serverName :: String,
                       cpus :: Integer}

--list of VMs
vms :: VMLoc VM
vms = fromList $ zip [0..] [VM "VM1" 100, VM "VM2" 50, VM "VM3" 15]

--list of servers
servers :: Map SID Server
servers = fromList $ zip [0..] [Server "Server1" 100, Server "Server2" 100, Server "Server3" 200]

--number of servers ON (which we'll try to minimize)
numberServersOn :: Map VMID SSID -> SInteger
numberServersOn = count . elems . M.map (./= 0) . vmCounts

--compute the number of VMs on each servers
vmCounts :: Map VMID SSID -> Map SID SInteger
vmCounts vmls = M.mapWithKey count servers where
   count sid _ = sum [ite (mysid .== literal sid) 1 0 | mysid <- elems vmls]

--All the CPU constraints
cpuConstraints :: Map VMID SSID -> SBool
cpuConstraints vmls = bAnd $ elems $ M.mapWithKey criteria (serverCPUHeights vmls) where
   criteria :: SID -> SInteger -> SBool
   criteria sid height = (literal $ cpus $ fromJust $ M.lookup sid servers) .> height

indexConstraint :: Map VMID SSID -> SBool
indexConstraint = bAll (flip inRange (0, 2)) . elems

--gives the CPU consummed by VMs for each server
serverCPUHeights :: Map VMID SSID -> Map SID SInteger 
serverCPUHeights vmls = M.mapWithKey sumVMsHeights servers where
   sumVMsHeights :: SID -> Server -> SInteger
   sumVMsHeights sid _ = sum [ite (sid' .== literal sid) (literal $ cpuDemand $ fromJust $ M.lookup vmid vms) 0 | (vmid, sid') <- M.assocs vmls]

--special version of minimize that works with our types
myMinimize :: [VMID] -> (Map VMID SSID -> SInteger) -> (Map VMID SSID -> SBool) -> IO (Maybe (Map VMID SID))
myMinimize vmids utility constraints = do
   ms <- minimize Quantified (utility . toVMLoc) (length vmids) (constraints . toVMLoc) 
   return $ toVMLoc <$> ms where
      toVMLoc :: [a] -> Map VMID a
      toVMLoc = fromList . zip vmids

--solves a VM placement problem
vmPlacementProblem :: IO (Maybe (Map VMID SID))
vmPlacementProblem = myMinimize (keys vms) numberServersOn (liftA2 (&&&) cpuConstraints indexConstraint)

main = do
   s <- vmPlacementProblem   
   putStrLn $ show s

-- count the true bits
count :: [SBool] -> SInteger
count []     = 0
count (x:xs) = let c' = count xs in ite x (1+c') c'

