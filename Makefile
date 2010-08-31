.PHONY: all
all:
	runhaskell -W Scripts.hs

Network/CCNx/Messages.hs Network/CCNx/CCNb.hs: GenMsg.hs
	runhaskell -W Scripts.hs

.PHONY: clean
clean:
	-#rm Network/CCNx/CCNb.hs
	-#rm Network/CCNx/Messages.hs

