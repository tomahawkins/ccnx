.PHONY: all
all: Network/CCNx/Messages.hs Network/CCNx/CCNb.hs

Network/CCNx/Messages.hs Network/CCNx/CCNb.hs: GenMsg.hs
	runhaskell -W GenMsg.hs

.PHONY: clean
clean:
	-#rm Network/CCNx/CCNb.hs
	-#rm Network/CCNx/Messages.hs

