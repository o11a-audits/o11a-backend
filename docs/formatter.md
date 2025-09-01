Example of the formatter output from a solidity file:
``` rust
pub const NATIVE_TOKEN;

pub immut factory;

pub immut holdingPeriodInSeconds;

pub mut pID;
pub mut pendingRewards;
pub mut totalReallocatedAmount;
pub mut accumulatedFees;
pub mut distributedRewards;

mut _manuallyDeactivated;

pub mut map(
  uint256 pID => Participation
) participations;

constructor(
  holdingPeriodInSeconds_,
  targetToken_,
  rewardToken_,
  rewardPPQ_,
  campaignAdmin,
  startTimestamp_,
  feeBps_,
  alternativeWithdrawalAddress_,
  campaignId_
) {
  if (
    rewardToken_ == address(0)
    || campaignAdmin == address(0)
  ) {
    revert InvalidCampaignSettings();
  }

  if (
    startTimestamp_ != 0
    && startTimestamp_
      <= block.timestamp
  ) {
    revert InvalidCampaignSettings();
  }

  factory =
    INudgeCampaignFactory(
      sender:
        msg.sender
    );

  Should declarations and references
  always be on different lines so we
  can show their info inline?

  targetToken =
    targetToken_;
  rewardToken =
    rewardToken_;
  campaignId =
    campaignId_;

  let targetDecimals =
    targetToken_
      == NATIVE_TOKEN
    ? 18
    : IERC20Metadata(
      token:
        targetToken_
    )
      .decimals();
  let rewardDecimals =
    rewardToken_
      == NATIVE_TOKEN
    ? 18
    : IERC20Metadata(
      token:
        rewardToken_
    )
      .decimals();

  targetScalingFactor =
    10
    ** (
      18
      - targetDecimals
    );
  rewardScalingFactor =
    10
    ** (
      18
      - rewardDecimals
    );

  _grantRole(
    role:
      CAMPAIGN_ADMIN_ROLE,
    addr:
      campaignAdmin
  );

  startTimestamp =
    startTimestamp_ == 0
    ? block.timestamp
    : startTimestamp_;
  isCampaignActive =
    startTimestamp <=
      block.timestamp;

  _manuallyDeactivated = false;
  rewardPPQ =
    rewardPPQ_;
  holdingPeriodInSeconds =
    holdingPeriodInSeconds_;
  feeBps =
    feeBps_;
  alternativeWithdrawalAddress =
    alternativeWithdrawalAddress_;
}

modifier whenNotPaused() {
  if (
    factory
      .isCampaignPaused(
        campaignAddr:
          address(this)
      )
  ) {
    revert CampaignPaused();
  }
  _;
}
```
